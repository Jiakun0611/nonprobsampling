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

  m_raw <- trimws(tolower(as.character(x$method %||% "")))
  m <- if (m_raw %in% c("alp", "clw")) toupper(m_raw) else m_raw

  cat("\nPseudo-weighted (", m, ") Estimators:\n", sep = "")

  for (i in seq_len(nrow(x$domains))) {

    d <- x$domains[i, ]

    cat(sprintf("  Domain: %s\n", d$domain))
    cat(sprintf("  %-15s %10.6f\n", "Mean:",       d$adjusted_mean))
    cat(sprintf("  %-15s %10.6f\n", "Std. Error:", d$adjusted_se))
    cat(sprintf("  %-15s [%0.6f, %0.6f]\n",
                "95% CI:", d$adjusted_lower, d$adjusted_upper))

    if (i < nrow(x$domains)) cat("\n")
  }

  invisible(x)
}


