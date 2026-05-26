#' Summarize a Pseudo-Weight Fit
#'
#' @param object An object of class \code{"pw_fit"}, returned by \code{\link{est_pw}}.
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns \code{object}.
#'
#' @export
summary.pw_fit <- function(object, ...) {

  cat("Call:\n")
  print(object$call)

  method <- object$method

  if (method %in% c("alp", "clw", "calibration")) {
    if (method == "alp")         method <- "ALP"
    if (method == "clw")         method <- "CLW"
    if (method == "calibration") method <- "calibration"

    cat("\nMethod: One reference", method, "\n")

  } else if (method == "multi") {

    cat("\nMethod: Multi-reference calibration\n")

    log <- object$internal$log_messages
    if (!is.null(log) && length(log) > 0) {
      cat(log, sep = "")
    }
  }

  # --- Model information ---
  if (!is.null(object$coefficients)) {
    cat("\nParticipation model involves the following variables:\n")
    cat(names(object$coefficients)[-1], "\n\n")
  }

  # --- Solver diagnostics ---
  diag <- object$solver_diagnostics

  if (!is.null(diag)) {
    cat("Solver diagnostics:\n")
    cat("  Solver:", diag$solver, "\n")

    if (!is.null(diag$method)) {
      cat("  Method:", diag$method, "\n")
    }

    cat("  Termination code:", diag$termcd, "\n")
    cat("  Iterations:", diag$iter, "\n")
    cat(
      "  Max |estimating equation|:",
      formatC(diag$fmax, format = "e", digits = 3),
      "\n"
    )

    if (!is.null(diag$message)) {
      cat("  Message:", diag$message, "\n")
    }
  }

  # --- Coefficients section ---
  if (!is.null(object$coefficients)) {
    cat("\nParticipation model coefficients:\n")

    df <- rbind(object$coefficients)
    rownames(df) <- NULL

    formatted_df <- format(round(df, 4), justify = "left", width = 10)
    print(as.data.frame(formatted_df), row.names = FALSE)
  }

  # --- na summary ---
  na_sum <- object$na_summary
  if (!is.null(na_sum)) {
    lines <- character(0)

    if (na_sum$sc$n_excluded > 0) {
      n <- na_sum$sc$n_excluded
      lines <- c(lines, sprintf("(%d observation%s deleted due to missingness in sc)",
                                n, if (n != 1L) "s" else ""))
    }

    if (na_sum$n_ref == 1) {
      if (na_sum$sp$n_excluded > 0) {
        n <- na_sum$sp$n_excluded
        lines <- c(lines, sprintf("(%d observation%s deleted due to missingness in sp)",
                                  n, if (n != 1L) "s" else ""))
      }
    } else {
      for (i in seq_along(na_sum$sp)) {
        if (na_sum$sp[[i]]$n_excluded > 0) {
          n <- na_sum$sp[[i]]$n_excluded
          lines <- c(lines, sprintf("(%d observation%s deleted due to missingness in sp[[%d]])",
                                    n, if (n != 1L) "s" else "", i))
        }
      }
    }

    if (length(lines) > 0) {
      cat("\n", paste(lines, collapse = "\n"), "\n", sep = "")
    }
  }

  invisible(object)
}
