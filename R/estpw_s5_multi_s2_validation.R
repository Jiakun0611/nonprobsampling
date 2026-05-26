#' Validate inputs and build variable sets for multi-reference calibration
#'
#' @param sc Data frame. The nonprobability sample.
#' @param sp_list List of reference sample data frames.
#' @param vars_list List of character vectors of predictor variable names
#'   shared between `sc` and each reference sample.
#' @param wts_cols Character vector of survey-weight column names,
#'   one per element of `sp_list`.
#' @param verbose Logical. If `TRUE`, variable-set messages are printed.
#'
#' @return A list with components:
#'   \item{sc}{The nonprobability sample data frame (unchanged).}
#'   \item{sp_list}{The reference sample list (unchanged).}
#'   \item{vars_XC}{Character vector: union of all per-sample variable sets,
#'     used to build the `sc` design matrix `Xc`.}
#'   \item{xcol}{List of character vectors: the variables contributed by each
#'     reference sample after removing variables already covered by earlier
#'     samples (no intercept).}
#'   \item{wts_cols}{Character vector of weight column names (unchanged).}
#'   \item{log}{Character vector of log messages describing the variable sets.}
#'
#' @keywords internal
check_input_multi <- function(sc,
                              sp_list,
                              vars_list,
                              wts_cols,
                              verbose = FALSE) {

  log_messages <- character()

  # always use positional indices for display
  spn <- paste0("sp[[", seq_along(sp_list), "]]")


  # ---- ensure all three inputs have the same length ----
  if (length(sp_list) != length(vars_list) ||
      length(sp_list) != length(wts_cols)) {
    stop("Error: sp_list, vars_list, and wts_cols must all be the same length.")
  }

  # ============================================================
  #              PRINT SHARED VARIABLES PER SAMPLE
  # ============================================================
  for (i in seq_along(sp_list)) {
    msg <- sprintf(
      "Shared variables in %s:\n  %s\n",
      spn[i],
      paste(vars_list[[i]], collapse = ", ")
    )
    log_messages <- c(log_messages, msg)
  }


  # ============================================================
  #             BUILD xcol (NEW VARIABLES PER SAMPLE)
  # ============================================================
  n    <- length(vars_list)
  xcol <- vector("list", n)
  xcol[[1]] <- vars_list[[1]]

  if (n > 1) {
    for (i in seq.int(2, n)) {
      prev_vars <- Reduce(union, xcol[1:(i - 1)])
      new_vars  <- setdiff(vars_list[[i]], prev_vars)

      if (length(new_vars) == 0) {
        stop(sprintf("%s has no new variables; please remove this sample.", spn[i]))
      }
      xcol[[i]] <- new_vars
    }
  }

  for (i in seq_along(xcol)) {
    msg <- sprintf(
      "Variables used for calculation in %s:\n  %s\n",
      spn[i],
      paste(xcol[[i]], collapse = ", ")
    )
    log_messages <- c(log_messages, msg)
  }


  # ---- union of all xcol sets ----
  vars_XC <- Reduce(union, xcol)

  # ---- return validated objects ----
  list(
    sc       = sc,
    sp_list  = sp_list,
    vars_XC  = vars_XC,
    xcol     = xcol,
    wts_cols = wts_cols,
    log      = log_messages
  )
}
