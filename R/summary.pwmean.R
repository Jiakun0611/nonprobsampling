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
  summary_pwmean_impl(
    object,
    group_label = "Domain",
    group_col = "domain",
    estimate_label = "Mean",
    estimate_col = "mean"
  )
}

#' Summary method for pwmean objects with categorical outcomes
#'
#' Provides console output for objects of class \code{"pwmean_factor"},
#' including unweighted and pseudo-weighted prevalence estimates, standard
#' errors, and confidence intervals.
#'
#' @param object An object of class \code{"pwmean_factor"}, returned by
#'   \code{\link{pwmean}} when \code{y} is a factor.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{object}.
#'
#' @method summary pwmean_factor
#' @export
summary.pwmean_factor <- function(object, ...) {
  summary_pwmean_impl(
    object,
    group_label = "Category",
    group_col = "category",
    estimate_label = "Prevalence",
    estimate_col = "prevalence"
  )
}

summary_pwmean_impl <- function(
    object,
    group_label,
    group_col,
    estimate_label,
    estimate_col
) {

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

  d <- object$estimates

  has_category <- "category" %in% names(d)

  if (nrow(d) == 1L && !has_category) {

    cat(sprintf("\n%s: %s\n", group_label, d$domain))

    cat("\nUnweighted estimators:\n")
    cat(sprintf("  %-15s %10.6f\n", paste0(estimate_label, ":"), d$unweighted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:",                d$unweighted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$unweighted_lower, d$unweighted_upper))

    cat("\nPseudo-weighted (", method_short, ") estimators:\n", sep = "")
    cat(sprintf("  %-15s %10.6f\n", paste0(estimate_label, ":"), d$adjusted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:",                d$adjusted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$adjusted_lower, d$adjusted_upper))

  } else {

    if (has_category) {
      uw_tab <- data.frame(
        d$category,
        d$domain,
        d$unweighted_mean,
        d$unweighted_se,
        d$unweighted_lower,
        d$unweighted_upper,
        stringsAsFactors = FALSE
      )
      names(uw_tab) <- c("category", "domain", estimate_col, "se", "CI_lower", "CI_upper")
    } else {
      uw_tab <- data.frame(
        d$domain,
        d$unweighted_mean,
        d$unweighted_se,
        d$unweighted_lower,
        d$unweighted_upper,
        stringsAsFactors = FALSE
      )
      names(uw_tab) <- c(group_col, estimate_col, "se", "CI_lower", "CI_upper")
    }

    cat("\nUnweighted estimators:\n")
    print(uw_tab, row.names = FALSE)

    if (has_category) {
      ad_tab <- data.frame(
        d$category,
        d$domain,
        d$adjusted_mean,
        d$adjusted_se,
        d$adjusted_lower,
        d$adjusted_upper,
        stringsAsFactors = FALSE
      )
      names(ad_tab) <- c("category", "domain", estimate_col, "se", "CI_lower", "CI_upper")
    } else {
      ad_tab <- data.frame(
        d$domain,
        d$adjusted_mean,
        d$adjusted_se,
        d$adjusted_lower,
        d$adjusted_upper,
        stringsAsFactors = FALSE
      )
      names(ad_tab) <- c(group_col, estimate_col, "se", "CI_lower", "CI_upper")
    }

    cat("\nPseudo-weighted (", method_short, ") estimators:\n", sep = "")
    print(ad_tab, row.names = FALSE)
  }

  na_act <- na.action(object)
  if (!is.null(na_act))
    cat("\n(", stats::naprint(na_act), ")\n", sep = "")

  invisible(object)
}
