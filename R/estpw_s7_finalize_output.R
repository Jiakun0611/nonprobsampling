#' Finalize and assemble the `pw_fit` return object
#'
#' Attaches the reconstructed sample data frame, the pseudo-weight vector, and
#' NA-handling metadata to `result`, then assigns the `"pw_fit"`
#' class so that downstream S3 methods can dispatch on it.
#'
#' @param result A list accumulating outputs from the estimation pipeline.
#' @param sc_out The reconstructed sample data frame produced by
#'   `reconstruct_sc_output()`.
#' @param sc0 The original (pre-NA-removal) sample data frame, stored for
#'   diagnostic access via `result$internal$raw_sc`.
#' @param sc_wname Name of the pseudo-weight column in `sc_out`.
#' @param na_mode Character string describing how NAs were handled; stored in
#'   `result$internal$na`.
#' @param keep_sc Logical vector of retained rows; stored in
#'   `result$internal$na`.
#' @param na_action_obj The `na.action` attribute from NA removal; stored
#'   in `result$internal$na`.
#'
#' @return `result` with `sc_updated`, `pseudo_weights`, and
#'   `internal` fields populated, and class set to `"pw_fit"`.
#'
#' @keywords internal
finalize_pw_fit <- function(result, sc_out, sc0, sc_wname, na_mode, keep_sc, na_action_obj) {

  result$sc_updated     <- sc_out
  # For na_mode "exclude", this expands pseudo_weights to full length with NAs
  # for dropped rows; for other modes the value is unchanged from Step 5.
  result$pseudo_weights <- sc_out[[sc_wname]]

  result$internal$raw_sc <- sc0
  result$internal$na <- list(
    na_mode       = na_mode,
    keep_sc       = keep_sc,
    na_action_obj = na_action_obj
  )

  class(result) <- "pw_fit"

  result
}
