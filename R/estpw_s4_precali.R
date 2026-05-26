#' Cumulative pre-calibration across multiple probability samples
#'
#' Performs a cumulative pre-calibration across multiple probability samples by
#' sequentially calibrating each sample's weights to align with the marginal
#' totals of the previous samples. Called internally by \code{est_pw()}.
#'
#' @param sp_raw A list of data frames (raw reference samples).
#' @param sp_new A list of data frames (working reference samples) with the same
#'   structure as \code{sp_raw}. Calibrated weights are written back in place.
#' @param weight A list of weight column name strings, one per sample.
#' @param sp_order \code{"size"} to process largest sample first; otherwise
#'   the original list order is used.
#' @return A list with components \code{sp_new}, \code{total_vector},
#'   \code{log_messages}, and \code{order_used}.
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

  .svy_design_from_X <- function(X, w) {
    dat <- as.data.frame(X)
    dat$.w <- w
    survey::svydesign(ids = ~1, weights = ~.w, data = dat)
  }

  .population_vector <- function(ds, cols) {
    total_w <- sum(stats::weights(ds))
    if (length(cols) == 0) return(c("(Intercept)" = total_w))
    fml  <- stats::as.formula(paste0("~", paste(cols, collapse = " + ")))
    marg <- stats::coef(survey::svytotal(fml, ds))
    c("(Intercept)" = total_w, marg)
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

  X_ref  <- .expand_raw(ref_raw, ref_wname)
  ds_ref <- .svy_design_from_X(X_ref, ref_new[[ref_wname]])
  total_vec <- .population_vector(ds_ref, colnames(X_ref))

  ref_label <- paste0("sp[[", ord[1], "]]")

  s <- if (sort_by_size) "largest" else "first"

  msg <- sprintf(
    "\nPre-calibration summary:\nNon-calibrated sample (%s): %s\n",
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

      shared_cols <- intersect(colnames(X_k), setdiff(names(total_vec), "(Intercept)"))
      shared_cols <- sort(shared_cols)

      ds_k <- .svy_design_from_X(X_k, df_new[[wname]])

      pops <- c("(Intercept)" = unname(total_vec["(Intercept)"]))
      if (length(shared_cols) > 0) pops <- c(pops, total_vec[shared_cols])

      fml <- if (length(shared_cols) > 0) {
        stats::as.formula(paste0("~", paste(shared_cols, collapse = " + ")))
      } else {
        ~1
      }

      ds_k_cal <- survey::calibrate(
        design     = ds_k,
        formula    = fml,
        population = pops
      )

      sp_new_ord[[k]][[wname]] <- as.numeric(stats::weights(ds_k_cal))

      # append totals for NEW columns (using calibrated weights)
      current_cols <- setdiff(names(total_vec), "(Intercept)")
      new_cols <- setdiff(colnames(X_k), current_cols)
      new_cols <- sort(new_cols)

      if (length(new_cols) > 0) {
        ds_k2 <- .svy_design_from_X(X_k, sp_new_ord[[k]][[wname]])
        add_vec <- .population_vector(ds_k2, new_cols)
        add_vec <- add_vec[names(add_vec) != "(Intercept)"]
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
