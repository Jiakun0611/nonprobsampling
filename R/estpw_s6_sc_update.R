#' Reconstruct the sample data frame with fitted pseudo-weights
#'
#' Inserts the fitted pseudo-weights `w_fit` back into the original sample
#' data frame `sc0`, using the NA handling strategy used during
#' estimation. For `"omit"`, only the rows that were kept are returned.
#' For `"exclude"`, all original rows are returned with NA weights
#' for rows that were dropped. For `"fail"` or `"pass"`, all rows
#' are assumed to be present and weights are attached directly.
#'
#' @param sc0 The original sample data frame (before any NA removal).
#' @param w_fit Numeric vector of fitted pseudo-weights, one per kept row.
#' @param keep_sc Logical vector identifying which rows of `sc0` were retained
#'   for fitting.
#' @param na_mode Character string; one of `"omit"`, `"exclude"`,
#'   `"fail"`, or `"pass"`.
#' @param na_action_obj The `na.action` attribute produced during NA
#'   removal, or NULL.
#' @param sc_wname Name of the column in which pseudo-weights will be stored.
#'
#' @return A data frame derived from `sc0` with a pseudo-weight column
#'   named `sc_wname`.
#'
#' @keywords internal
reconstruct_sc_output <- function(sc0, w_fit, keep_sc, na_mode, na_action_obj, sc_wname) {

  n0     <- nrow(sc0)
  n_fit  <- length(w_fit)
  n_keep <- sum(keep_sc)

  if (n_fit != n_keep) {
    stop(
      sprintf(
        "Length mismatch: w_fit has %d values but keep_sc has %d indices.",
        n_fit, n_keep
      ),
      call. = FALSE
    )
  }

  if (na_mode == "omit") {
    sc_out <- sc0[keep_sc, , drop = FALSE]
    sc_out[[sc_wname]] <- w_fit

  } else if (na_mode == "exclude") {
    w_full <- rep(NA_real_, n0)
    w_full[keep_sc] <- w_fit
    sc_out <- sc0
    sc_out[[sc_wname]] <- w_full
    if (!is.null(na_action_obj)) {
      attr(sc_out, "na.action") <- na_action_obj
    }

  } else {
    # "fail" or "pass": no rows were removed
    if (n_fit != n0) {
      stop(
        sprintf(
          "For na_mode '%s', expected w_fit length %d to equal nrow(sc0) = %d.",
          na_mode, n_fit, n0
        ),
        call. = FALSE
      )
    }
    sc_out <- sc0
    sc_out[[sc_wname]] <- w_fit
  }

  sc_out
}
