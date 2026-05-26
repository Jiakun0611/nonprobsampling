#' Handle missing values at the build stage (first layer)
#'
#' Process NA for `est_pw()` before estimation step.
#' Resolves the `na.action` argument to a standard mode string, applies NA
#' filtering to `sc` and `sp`, subsets the survey design object(s) in
#' `sp_des` to match the filtered `sp`, and produces a summary of
#' exclusions.
#'
#' @param sc A data frame. The nonprobability sample (before NA removal).
#' @param sp A data frame (one-reference case) or a named list of data frames
#'   (multi-reference case). Analysis data extracted from the reference
#'   survey design(s).
#' @param sp_des A single survey design object (one-reference case) or a
#'   named list of survey design objects (multi-reference case). Subsetted
#'   to align with the filtered `sp`.
#' @param p_formula A one-sided formula (one-reference case) or a list of
#'   one-sided formulas (multi-reference case) specifying the participation
#'   model variables used to identify rows with missing values.
#' @param na.action A function (`stats::na.omit`, `stats::na.exclude`,
#'   `stats::na.fail`, or `stats::na.pass`), an equivalent character string,
#'   or `NULL` (which inherits from `getOption("na.action")`).
#' @param n_ref Integer. Number of reference surveys. Controls whether
#'   one-reference or multi-reference logic is applied when subsetting
#'   `sp_des` and computing `n_sp_orig`.
#' @param verbose Logical. If `TRUE`, prints per-dataset row counts and
#'   exclusion totals via `message()`.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{`na_mode`}{Character string: one of `"omit"`, `"exclude"`,
#'     `"fail"`, or `"pass"`.}
#'   \item{`sc`}{The cleaned nonprobability sample data frame.}
#'   \item{`sp`}{The cleaned reference survey data frame(s).}
#'   \item{`sp_des`}{The subsetted survey design object(s).}
#'   \item{`keep_sc`}{Logical vector indicating which rows of the original
#'     `sc` are retained.}
#'   \item{`keep_sp`}{Logical vector (one-reference) or list of logical
#'     vectors (multi-reference) indicating which rows of each `sp` are
#'     retained.}
#'   \item{`n_sp_orig`}{Original row count(s) of `sp_des` before
#'     subsetting.}
#'   \item{`na_action_obj`}{An lm-style `na.action` attribute for `sc`,
#'     or `NULL` if no rows were removed.}
#'   \item{`log_messages`}{Character vector of per-variable NA detail
#'     messages from `handle_na_for_ipwm()`, suitable for appending to
#'     the running `log_messages` in `est_pw()`.}
#'   \item{`na_summary`}{A `pw_na_summary` object with row counts before
#'     and after NA removal, or `NULL` if no rows were excluded.}
#' }
#'
#' @keywords internal
process_na_build <- function(sc, sp, sp_des, p_formula, na.action, n_ref, verbose = FALSE) {

  na_mode <- resolve_na_action(na.action)

  na_res <- handle_na_for_ipwm(
    sc        = sc,
    sp        = sp,
    p_formula = p_formula,
    na_mode   = na_mode
  )

  # Capture original sp size(s) before subsetting sp_des
  if (n_ref == 1) {
    n_sp_orig <- nrow(sp_des$variables)
    keep_sp   <- na_res$keep_sp
    if (!all(keep_sp)) sp_des <- sp_des[keep_sp, ]
  } else {
    n_sp_orig <- vapply(sp_des, function(d) nrow(d$variables), integer(1))
    keep_sp   <- na_res$keep_sp
    for (.i in seq_along(sp_des)) {
      if (!all(keep_sp[[.i]])) sp_des[[.i]] <- sp_des[[.i]][keep_sp[[.i]], ]
    }
  }

  result <- list(
    na_mode       = na_mode,
    sc            = na_res$sc,
    sp            = na_res$sp,
    sp_des        = sp_des,
    keep_sc       = na_res$keep_sc,
    keep_sp       = keep_sp,
    n_sp_orig     = n_sp_orig,
    na_action_obj = na_res$na_action,
    log_messages  = na_res$log
  )

  result$na_summary <- .report_na_exclusions(result, nrow(sc), n_ref, verbose = verbose)

  result
}
