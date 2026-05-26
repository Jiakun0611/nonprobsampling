#' Summary method for pwmean objects
#'
#' Provides console output for objects of class \code{"pwmean"}, including
#' unweighted and pseudo-weighted mean estimates, standard errors, confidence
#' intervals, and optional domain-level summaries.
#'
#' @param object An object of class \code{"pwmean"}, returned by
#'   \code{\link{pwmean}}.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{object}.
#'
#' @method summary pwmean
#' @export
summary.pwmean <- function(object, ...) {

  if (!is.null(object$call)) {
    cat("Call:\n")
    print(object$call)
  } else {
    cat("Call:\n  (not available)\n")
  }

  method_raw <- trimws(as.character(object$method %||% ""))
  method_key <- tolower(method_raw)

  if (method_key == "alp") {
    method_label <- "One reference ALP"
    method_short <- "ALP"
  } else if (method_key == "clw") {
    method_label <- "One reference CLW"
    method_short <- "CLW"
  } else if (method_key %in% c("cali", "calibration")) {
    method_label <- "One reference calibration"
    method_short <- "calibration"
  } else if (method_key == "multi") {
    method_label <- "Multi-reference calibration"
    method_short <- "calibration"
  } else {
    method_label <- method_raw
    method_short <- method_raw
  }

  cat("\nMethod:", method_label, "\n")

  d <- object$domains

  if (nrow(d) == 1L) {

    cat(sprintf("\nDomain: %s\n", d$domain))

    cat("\nUnweighted estimators:\n")
    cat(sprintf("  %-15s %10.6f\n", "Mean:",       d$unweighted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:", d$unweighted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$unweighted_lower, d$unweighted_upper))

    cat("\nPseudo-weighted (", method_short, ") estimators:\n", sep = "")
    cat(sprintf("  %-15s %10.6f\n", "Mean:",       d$adjusted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:", d$adjusted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$adjusted_lower, d$adjusted_upper))

  } else {

    uw_tab <- data.frame(
      domain   = d$domain,
      mean     = d$unweighted_mean,
      se       = d$unweighted_se,
      CI_lower = d$unweighted_lower,
      CI_upper = d$unweighted_upper,
      stringsAsFactors = FALSE
    )

    cat("\nUnweighted estimators:\n")
    print(uw_tab, row.names = FALSE)

    ad_tab <- data.frame(
      domain   = d$domain,
      mean     = d$adjusted_mean,
      se       = d$adjusted_se,
      CI_lower = d$adjusted_lower,
      CI_upper = d$adjusted_upper,
      stringsAsFactors = FALSE
    )

    cat("\nPseudo-weighted (", method_short, ") estimators:\n", sep = "")
    print(ad_tab, row.names = FALSE)
  }

  na_act <- na.action(object)
  if (!is.null(na_act))
    cat("\n(", stats::naprint(na_act), ")\n", sep = "")

  invisible(object)
}
