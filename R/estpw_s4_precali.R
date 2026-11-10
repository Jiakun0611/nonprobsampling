#' Cumulative precalibration across multiple probability samples
#'
#' Performs a cumulative precalibration across multiple probability samples by
#' sequentially calibrating each sample's weights to align with the marginal
#' totals accumulated from earlier samples. The first sample supplies initial
#' targets. Later samples match shared targets and contribute new targets using
#' their calibrated weights. Variables need not occur in every sample. When no
#' columns are shared with accumulated targets, only the sum of weights is
#' calibrated. Previously established targets are retained, so processing order
#' can affect the results. Called internally by \code{est_pw()}.
#'
#' @param sp_raw A list of data frames containing each reference sample's own
#'   formula variables and weight column, before model matrix expansion.
#' @param sp_new A list of data frames (working reference samples) with the same
#'   row order as \code{sp_raw}, containing model matrix columns and weights.
#'   Calibrated weights are written back in the returned list.
#' @param weight A character vector of weight column names, one per sample.
#' @param sp_order \code{"size"} to process largest sample first; otherwise
#'   the original list order is used.
#' @return A list with components \code{sp_new}, \code{total_vector},
#'   \code{log_messages}, and \code{order_used}. Totals are named using the
#'   original model matrix column names, with \code{(Intercept)} for the sum
#'   of weights.
#'
#' @importFrom survey svydesign svytotal calibrate
#' @importFrom stats model.matrix as.formula weights coef
#' @keywords internal

precal_cumulative_order <- function(sp_raw, sp_new, weight, sp_order) {

  sort_by_size <- (sp_order == "size")

  log_messages <- character()

  # ---- order control ----
  sizes <- vapply(sp_raw, nrow, integer(1))
  ord <- if (sort_by_size) order(sizes, decreasing = TRUE) else seq_along(sp_raw)

  sp_raw_ord <- sp_raw[ord]
  sp_new_ord <- sp_new[ord]
  w_ord      <- weight[ord]
  # ---- helpers ----
  .get_wname <- function(w) {
    if (length(w) != 1 || !is.character(w) || !nzchar(w)) {
      stop("Each weight[[i]] must be a single non-empty character string.")
    }
    w
  }

  .expand_raw <- function(df, wname) {
    vars <- setdiff(names(df), wname)
    if (length(vars) == 0) {
      return(matrix(, nrow = nrow(df), ncol = 0,
                    dimnames = list(NULL, character(0))))
    }
    X <- stats::model.matrix(~ ., data = df[, vars, drop = FALSE])
    if ("(Intercept)" %in% colnames(X)) X <- X[, -1, drop = FALSE]
    X
  }

  .pretty_cols <- function(cols, max_show = 30) {
    cols <- sort(unique(cols))
    if (length(cols) == 0) return("survey weights total only")
    if (length(cols) <= max_show) {
      return(paste0("survey weights total, ", paste(cols, collapse = ", ")))
    }
    paste0("survey weights total, ",
           paste(cols[1:max_show], collapse = ", "),
           ", ... (", length(cols), " cols)")
  }

  # ---- Step 0: init target totals from reference RAW sample ----
  ref_raw   <- sp_raw_ord[[1]]
  ref_new   <- sp_new_ord[[1]]
  ref_wname <- .get_wname(w_ord[[1]])

  X_ref <- .expand_raw(ref_raw, ref_wname)
  ref_weights <- ref_new[[ref_wname]]
  total_vec <- c("(Intercept)" = sum(ref_weights), colSums(X_ref * ref_weights))

  ref_label <- paste0("sp[[", ord[1], "]]")

  s <- if (sort_by_size) "largest" else "first"

  msg <- sprintf(
    "\nPrecalibration summary:\nNon-calibrated sample (%s): %s\n",
    s, ref_label
  )

  log_messages <- c(log_messages, msg)

  # ---- Step 1..K: calibrate remaining samples ----
  if (length(sp_raw_ord) >= 2) {
    for (k in 2:length(sp_raw_ord)) {

      df_raw <- sp_raw_ord[[k]]
      df_new <- sp_new_ord[[k]]
      wname  <- .get_wname(w_ord[[k]])

      X_k <- .expand_raw(df_raw, wname)

      current_cols <- setdiff(names(total_vec), "(Intercept)")
      shared_cols <- sort(intersect(colnames(X_k), current_cols))

      # Use original names for matching totals and temporary names in the formula.
      cal_data <- as.data.frame(X_k[, shared_cols, drop = FALSE])
      cal_names <- sprintf(".pc_x%d", seq_along(shared_cols))
      names(cal_data) <- cal_names
      ds_k <- survey::svydesign(
        ids = ~1,
        weights = df_new[[wname]],
        data = cal_data
      )

      pops <- total_vec[c("(Intercept)", shared_cols)]
      names(pops) <- c("(Intercept)", cal_names)
      fml <- if (length(cal_names) > 0L) {
        stats::reformulate(cal_names)
      } else {
        ~1
      }

      ds_k_cal <- survey::calibrate(
        design     = ds_k,
        formula    = fml,
        population = pops
      )

      cal_weights <- as.numeric(stats::weights(ds_k_cal))
      sp_new_ord[[k]][[wname]] <- cal_weights

      # Add totals for new columns using the calibrated weights.
      new_cols <- sort(setdiff(colnames(X_k), current_cols))
      if (length(new_cols) > 0) {
        add_vec <- colSums(X_k[, new_cols, drop = FALSE] * cal_weights)
        total_vec <- c(total_vec, add_vec)
      }

      k_label <- paste0("sp[[", ord[k], "]]")
      msg <- sprintf(
        "Calibrated sample: %s\n  Calibration variables: %s\n",
        k_label, .pretty_cols(shared_cols)
      )
      log_messages <- c(log_messages, msg)
    }
  }

  # ---- restore original order only if we reordered ----
  sp_back <- if (sort_by_size) {
    out <- vector("list", length(sp_new))
    out[ord] <- sp_new_ord
    names(out) <- names(sp_new)
    out
  } else {
    sp_new_ord
  }

  list(
    sp_new       = sp_back,
    total_vector = total_vec,
    log_messages = log_messages,
    order_used   = ord
  )
}
