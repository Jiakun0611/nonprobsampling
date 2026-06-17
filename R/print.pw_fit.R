#' Print method for pw_fit objects
#'
#' Compact one-screen overview of a fitted pseudo-weight object: the call, the
#' pseudo-weighting method, the participation model size, solver convergence,
#' and a summary of the estimated pseudo-weights. For the full coefficient
#' table and detailed solver diagnostics, use \code{\link{summary.pw_fit}}.
#'
#' @param x An object of class \code{"pw_fit"}, returned by \code{\link{est_pw}}.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print pw_fit
#' @export
print.pw_fit <- function(x, ...) {

  cat("Pseudo-weight fit (\"pw_fit\")\n\n")

  # --- Call ---
  cat("Call:\n")
  if (!is.null(x$call)) print(x$call) else cat("  (not available)\n")
  cat("\n")

  # --- Method ---
  m <- tolower(trimws(as.character(x$method %||% "")))
  method_label <- if (m == "alp") {
    "One reference ALP"
  } else if (m == "clw") {
    "One reference CLW"
  } else if (m %in% c("cali", "calibration")) {
    "One reference calibration"
  } else if (m == "multi") {
    "Multi-reference calibration"
  } else {
    x$method %||% "(unknown)"
  }
  cat(sprintf("%-21s %s\n", "Method:", method_label))

  # --- Participation model size (parameter count) ---
  if (!is.null(x$coefficients)) {
    p <- length(x$coefficients)
    cat(sprintf(
      "%-21s %d parameters%s\n",
      "Participation model:", p,
      if (m == "multi") "" else " (incl. intercept)"
    ))
  }

  # --- Convergence ---
  diag <- x$solver_diagnostics
  if (!is.null(diag)) {
    tc <- suppressWarnings(as.integer(diag$termcd))
    status <- if (!is.na(tc) && tc == 1L) {
      "converged"
    } else if (!is.na(tc) && tc %in% c(2L, 3L)) {
      "converged (tolerance warning)"
    } else {
      "not converged"
    }
    cat(sprintf(
      "%-21s %s  (%s, %s iter, max|EE| = %s)\n",
      "Convergence:", status,
      diag$solver %||% "?",
      format(as.integer(diag$iter %||% NA)),
      formatC(diag$fmax %||% NA_real_, format = "e", digits = 2)
    ))
  }

  # --- Pseudo-weights summary (six-number + sum) ---
  w <- x$pseudo_weights
  if (!is.null(w)) {
    n_na <- sum(is.na(w))
    w_ok <- w[!is.na(w)]
    n    <- length(w_ok)

    if (n > 0L) {
      cat(sprintf(
        "\nPseudo-weights (n = %d%s):\n",
        n, if (n_na > 0L) sprintf(", %d NA", n_na) else ""
      ))
      print(summary(w_ok))

      neg <- sum(w_ok < 0)
      cat(sprintf(
        "Sum: %s%s\n",
        format(round(sum(w_ok)), big.mark = ",", scientific = FALSE),
        if (neg > 0L) sprintf("   (%d negative weights)", neg) else ""
      ))
    }
  }

  # --- NA note (nonprobability sample only; sp details are in summary) ---
  na_sum <- x$na_summary
  if (!is.null(na_sum) && !is.null(na_sum$sc) && isTRUE(na_sum$sc$n_excluded > 0)) {
    k <- na_sum$sc$n_excluded
    cat(sprintf(
      "\n(%d observation%s deleted due to missingness in sc)\n",
      k, if (k != 1L) "s" else ""
    ))
  }

  cat("\nUse summary() for coefficients and diagnostics; pwmean() to estimate means.\n")

  invisible(x)
}
