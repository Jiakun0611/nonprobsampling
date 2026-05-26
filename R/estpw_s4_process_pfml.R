#' Process the participation model formula and build model matrices
#'
#' Converts `p_formula` and the cleaned data (`sc`, `sp`) into model
#' matrices ready for participation model estimation. Also performs
#' cumulative pre-calibration of reference survey weights in the
#' multi-reference case when `Pre.calibration = TRUE`.
#'
#' For both the one-reference and multi-reference cases the function:
#' \enumerate{
#'   \item Checks that all formula variables exist in `sc` and each `sp`.
#'   \item Verifies that factor levels of each variable are identical
#'     between `sc` and the corresponding `sp` (mismatched levels would
#'     silently produce different model matrix columns).
#'   \item Builds model matrices via `stats::model.matrix()` and drops
#'     the implicit intercept column.
#'   \item Checks that `sc` and `sp` produce the same number of model
#'     matrix columns, and aligns their column names.
#' }
#'
#' In the one-reference case `sc_new` contains only the model matrix
#' columns; `sp_new` contains the same columns plus the sampling weight
#' column.
#'
#' In the multi-reference case `sc_new` is the union of model matrix
#' columns across all reference surveys (duplicates removed by value
#' comparison). Each `sp_new[[j]]` contains the model matrix columns for
#' formula `j` plus the corresponding weight column. If
#' `Pre.calibration = TRUE` and there are at least two reference surveys,
#' `precal_cumulative_order()` is called to sequentially calibrate the
#' reference survey weights before estimation. The calibration variables
#' are the intersection of the variables appearing across all formulas in
#' `p_formula`, ensuring pre-calibration aligns reference surveys on
#' exactly the dimensions used in the participation models.
#'
#' @param sc A data frame. The nonprobability sample after NA processing.
#' @param sp A data frame (one-reference) or a named list of data frames
#'   (multi-reference). Reference survey analysis data after NA processing.
#' @param weight A single character string (one-reference) or character
#'   vector of the same length as `sp` (multi-reference) giving the
#'   sampling weight column name in each reference survey.
#' @param p_formula A one-sided formula (one-reference) or a list of
#'   one-sided formulas (multi-reference).
#' @param Pre.calibration Logical. Multi-reference case only. If `TRUE`,
#'   cumulative pre-calibration is applied to the reference survey weights
#'   via `precal_cumulative_order()`. Ignored in the one-reference case.
#' @param sp_order Character string, either `"size"` or `"given"`.
#'   Controls the processing order in `precal_cumulative_order()`.
#'   Ignored in the one-reference case.
#' @param verbose Logical. If `TRUE` and pre-calibration is skipped, a
#'   message is emitted recommending pre-calibration.
#'
#' @return A list with four elements:
#' \describe{
#'   \item{`sc`}{Data frame of model matrix columns for `sc`.}
#'   \item{`sp`}{Data frame (one-reference) or named list of data frames
#'     (multi-reference) of model matrix columns plus weight column(s)
#'     for each reference survey.}
#'   \item{`vars`}{Character vector of model matrix column names
#'     (one-reference), or a list of such vectors (multi-reference).}
#'   \item{`log_messages`}{Character vector of diagnostic messages from
#'     pre-calibration, or an empty vector in the one-reference case.}
#' }
#'
#' @keywords internal
#' @noRd
process_p_formula <- function(
    sc, sp, weight, p_formula,
    Pre.calibration = TRUE,
    sp_order = "size",
    verbose = FALSE
) {
  # -------------------------------------------------------------------
  # One-reference case
  # -------------------------------------------------------------------
  if (is.data.frame(sp)) {

    if (!inherits(p_formula, "formula")) {
      stop("'p_formula' must be a formula for one-reference case.", call. = FALSE)
    }

    vars_in_formula <- all.vars(p_formula)
    missing_sc <- setdiff(vars_in_formula, names(sc))
    missing_sp <- setdiff(vars_in_formula, names(sp))
    if (length(missing_sc) > 0 || length(missing_sp) > 0) {
      stop(paste0(
        "Missing variable(s): ",
        paste(unique(c(missing_sc, missing_sp)), collapse = ", ")
      ), call. = FALSE)
    }

    .check_factor_levels(sc, sp, vars_in_formula, ref_label = "sp")

    # Build model matrices (drop implicit intercept)
    Xc <- stats::model.matrix(p_formula, data = sc)
    Xp <- stats::model.matrix(p_formula, data = sp)
    if ("(Intercept)" %in% colnames(Xc)) Xc <- Xc[, -1, drop = FALSE]
    if ("(Intercept)" %in% colnames(Xp)) Xp <- Xp[, -1, drop = FALSE]

    vars <- colnames(Xc)
    if (ncol(Xp) != length(vars)) {
      stop(
        "sc and sp produced different model matrix columns -- check factor levels in p_formula variables.",
        call. = FALSE
      )
    }
    colnames(Xp) <- vars

    sc_new <- as.data.frame(Xc)

    if (!(weight %in% names(sp))) stop("Weight column not found in sp.", call. = FALSE)
    sp_new <- as.data.frame(Xp)
    sp_new[[weight]] <- sp[[weight]]


    return(list(
      sc = sc_new,
      sp = sp_new,
      vars = vars,
      log_messages = character()
    ))
  }

  # -------------------------------------------------------------------
  # Multi-reference case
  # -------------------------------------------------------------------
  if (!is.list(sp)) {
    stop("process_p_formula(): sp must be a data.frame or a list of data.frames.", call. = FALSE)
  }

  if (!(is.list(p_formula) && all(vapply(p_formula, inherits, logical(1), "formula")))) {
    stop("For multi-reference, 'p_formula' must be a list of formulas.", call. = FALSE)
  }
  if (length(p_formula) != length(sp)) {
    stop("For multi-reference, 'p_formula' must have the same length as 'sp'.", call. = FALSE)
  }
  if (!(is.character(weight) && length(weight) == length(sp))) {
    stop("For multi-reference, 'weight' must be a character vector with length = length(sp).", call. = FALSE)
  }


  sp_new <- vector("list", length(sp))
  vars_list <- vector("list", length(sp))
  sc_new <- NULL

  for (j in seq_along(sp)) {

    fml <- p_formula[[j]]
    vars_in_formula <- all.vars(fml)

    missing_sc <- setdiff(vars_in_formula, names(sc))
    missing_sp <- setdiff(vars_in_formula, names(sp[[j]]))
    if (length(missing_sc) > 0 || length(missing_sp) > 0) {
      stop(paste0(
        "For reference ", j, ", missing vars: ",
        paste(unique(c(missing_sc, missing_sp)), collapse = ", ")
      ), call. = FALSE)
    }

    .check_factor_levels(sc, sp[[j]], vars_in_formula, ref_label = paste0("sp[[", j, "]]"))

    Xc <- stats::model.matrix(fml, data = sc)
    Xp <- stats::model.matrix(fml, data = sp[[j]])
    if ("(Intercept)" %in% colnames(Xc)) Xc <- Xc[, -1, drop = FALSE]
    if ("(Intercept)" %in% colnames(Xp)) Xp <- Xp[, -1, drop = FALSE]

    if (ncol(Xp) != ncol(Xc)) {
      stop(sprintf(
        "Reference %d: sc and sp[[%d]] produced different model matrix columns -- check factor levels in p_formula variables.",
        j, j
      ), call. = FALSE)
    }
    colnames(Xp) <- colnames(Xc)

    # Merge covariates into sc_new without duplicates
    if (j == 1L) {
      sc_new <- as.data.frame(Xc)
    } else {
      for (nm in colnames(Xc)) {
        new_col <- Xc[, nm]
        duplicate <- any(vapply(sc_new, function(old_col) {
          if (is.numeric(old_col) && is.numeric(new_col)) {
            isTRUE(all.equal(old_col, new_col, tolerance = 1e-12))
          } else {
            identical(old_col, new_col)
          }
        }, logical(1)))
        if (!duplicate) sc_new[[nm]] <- new_col
      }
    }

    # Build sp_new[[j]]
    wj <- weight[j]

    if (!(wj %in% names(sp[[j]]))) {
      stop(sprintf("Weight column '%s' not found in sp[%d].", wj, j), call. = FALSE)
    }

    spj <- as.data.frame(Xp)
    spj[[wj]] <- sp[[j]][[wj]]

    sp_new[[j]] <- spj
    vars_list[[j]] <- colnames(Xp)
  }

  names(sp_new) <- if (!is.null(names(sp)) && all(nzchar(names(sp)))) {
    names(sp)
  } else {
    paste0("sp[[", seq_along(sp), "]]")
  }

  log_messages <- character()

  if (Pre.calibration && length(sp_new) > 1) {
    shared_vars <- Reduce(intersect, lapply(p_formula, all.vars))
    sp_for_precal <- lapply(seq_along(sp), function(j) {
      sp[[j]][, c(shared_vars, weight[j]), drop = FALSE]
    })

    out <- precal_cumulative_order(
      sp_raw   = sp_for_precal,
      sp_new   = sp_new,
      weight   = weight,
      sp_order = sp_order
    )
    sp_new <- out$sp_new
    log_messages <- c(log_messages, out$log_messages)
  } else {
    msg <- "Pre-calibration is recommended."
    log_messages <- c(log_messages, msg)
    if (verbose) message(msg)
  }

  return(list(
    sc = sc_new,
    sp = sp_new,
    vars = vars_list,
    log_messages = log_messages
  ))
}
