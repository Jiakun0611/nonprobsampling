#' Print method for pwmean objects
#'
#' Displays the pseudo-weighted mean estimate and its uncertainty.
#' For factor-like domain variables, prints one row per domain level.
#'
#' @param x An object of class \code{"pwmean"}, returned by \code{\link{pwmean}}.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print pwmean
#' @export
print.pwmean <- function(x, ...) {
  print_pwmean_impl(x, group_label = "Domain", estimate_label = "Mean")
}

#' Print method for pwmean objects with categorical outcomes
#'
#' Displays pseudo-weighted prevalence estimates and their uncertainty.
#'
#' @param x An object of class \code{"pwmean_factor"}, returned by
#'   \code{\link{pwmean}} when \code{y} is a factor.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print pwmean_factor
#' @export
print.pwmean_factor <- function(x, ...) {
  print_pwmean_impl(x, group_label = "Category", estimate_label = "Prevalence")
}

print_pwmean_impl <- function(x, group_label, estimate_label) {

  m_raw <- trimws(tolower(as.character(x$method %||% "")))
  m <- if (m_raw %in% c("alp", "clw")) toupper(m_raw) else m_raw

  cat("\nPseudo-weighted (", m, ") Estimators:\n", sep = "")

  for (i in seq_len(nrow(x$estimates))) {

    d <- x$estimates[i, ]

    if ("category" %in% names(d)) {
      cat(sprintf("  Category: %s\n", d$category))
      cat(sprintf("  Domain: %s\n", d$domain))
    } else {
      cat(sprintf("  %s: %s\n", group_label, d$domain))
    }
    cat(sprintf("  %-15s %10.6f\n", paste0(estimate_label, ":"), d$adjusted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:",               d$adjusted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$adjusted_lower, d$adjusted_upper))

    if (i < nrow(x$estimates)) cat("\n")
  }

  invisible(x)
}


