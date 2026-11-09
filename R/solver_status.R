#' Format the solver stopping reason and equation tolerance status
#'
#' @param diagnostics A list containing termcd, fmax, and ftol.
#' @return Two character strings for display.
#' @noRd
solver_status_lines <- function(diagnostics) {
  code <- diagnostics$termcd %||% NA_integer_
  reason <- switch(as.character(code),
    "1" = "Equation convergence criterion reached",
    "2" = "Parameter-step tolerance reached",
    "3" = "No better point found",
    "Unavailable"
  )

  status <- if (diagnostics$fmax <= diagnostics$ftol) {
    "Met"
  } else {
    "Not met"
  }

  c(paste("Termination reason:", reason), paste("Equation tolerance:", status))
}

report_solver_status <- function(sol) {
  message(
    paste(solver_status_lines(sol), collapse = "\n"),
    sprintf("\nIterations: %d (termcd = %d, max |eq| = %.3e, ftol = %.3e)",
            sol$iterations, sol$termcd, sol$fmax, sol$ftol)
  )
}
