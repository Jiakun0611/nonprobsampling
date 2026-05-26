#' Validate the Jacobian and estimating equation vector
#'
#' Checks that the Jacobian `J` and estimating equation vector `g`
#' are numerically valid and that `J` is square and full rank before
#' a Newton-Raphson step is taken.
#'
#' @param J Square numeric Jacobian matrix of dimension \eqn{p \times p}.
#' @param g Numeric estimating equation vector of length \eqn{p}.
#' @param label Character string used as a prefix in error messages to
#'   identify the calling method.
#' @param tol_singular Tolerance passed to `qr()` for detecting rank
#'   deficiency. Default is `1e-7`, matching R's `qr()` default.
#'
#' @return `invisible(TRUE)` if all checks pass; otherwise stops with
#'   an informative error message.
#'
#' @keywords internal
check_estimating_system <- function(J, g, label = "method",
                                    tol_singular = 1e-7) {
  # J: Jacobian / derivative matrix
  # g: estimating equation vector

  if (!is.matrix(J)) {
    stop(
      sprintf("[%s] Internal error: Jacobian is not a matrix.", label),
      call. = FALSE
    )
  }

  if (!is.numeric(J) || !is.numeric(g)) {
    stop(
      sprintf(
        "[%s] Internal error: Jacobian and estimating equation vector must be numeric.",
        label
      ),
      call. = FALSE
    )
  }

  if (nrow(J) != ncol(J)) {
    stop(
      sprintf(
        paste0(
          "[%s] Estimating system failed: Jacobian is not square ",
          "(%d x %d). The number of estimating equations ",
          "does not match the number of parameters."
        ),
        label, nrow(J), ncol(J)
      ),
      call. = FALSE
    )
  }

  if (length(g) != nrow(J)) {
    stop(
      sprintf(
        paste0(
          "[%s] Estimating system failed: estimating equation vector length (%d) ",
          "does not match Jacobian dimension (%d)."
        ),
        label, length(g), nrow(J)
      ),
      call. = FALSE
    )
  }

  if (any(!is.finite(J))) {
    stop(
      sprintf(
        paste0(
          "[%s] Estimating system failed: Jacobian contains NA, NaN, or Inf. ",
          "This may happen when the fitted weights become extremely large, ",
          "the linear predictor overflows, or the input data contain invalid values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  if (any(!is.finite(g))) {
    stop(
      sprintf(
        paste0(
          "[%s] Estimating system failed: estimating equation vector contains ",
          "NA, NaN, or Inf. This may happen when the fitted weights become ",
          "extremely large, the linear predictor overflows, or the input data ",
          "contain invalid values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  qJ <- qr(J, tol = tol_singular)
  rankJ <- qJ$rank
  p <- ncol(J)

  if (rankJ < p) {
    stop(
      sprintf(
        paste0(
          "[%s] Estimating system failed because the Jacobian is singular ",
          "or not identifiable (Jacobian rank = %d < %d).\n",
          "This means the model cannot determine a unique set of coefficients.\n",
          "Common reasons:\n",
          "  1. Redundant or perfectly collinear variables in p_formula.\n",
          "  2. Some categories are rare or absent in one of the samples.\n",
          "  3. There are too many variables or categories for the available data.\n",
          "  4. The variables almost perfectly distinguish the nonprobability sample ",
          "from the reference survey.\n",
          "Try simplifying p_formula, removing overlapping variables, or combining ",
          "rare factor levels."
        ),
        label, rankJ, p
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Assemble arguments for nleqslv
#'
#' Extracts and assembles the solver settings from a control object created
#' by `pw_solver_control()` into the format expected by `nleqslv::nleqslv()`.
#'
#' @param control A list created by `pw_solver_control()`.
#'
#' @return A list with components `method`, `global`, `xscalm`, and `control`
#'   (containing `ftol`, `xtol`, `maxit`, `trace`, and any user-supplied
#'   extras from `nleqslv_control`).
#'
#' @keywords internal
prepare_nleqslv_args <- function(control) {

  reserved <- c("x", "fn", "jac")
  bad <- intersect(names(control$nleqslv_control), reserved)

  if (length(bad) > 0L) {
    stop(
      sprintf(
        "`nleqslv_control` must not contain reserved argument(s): %s.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    method  = control$method,
    global  = control$global,
    xscalm  = control$xscalm,
    control = c(
      list(
        ftol  = control$ftol,
        xtol  = control$xtol,
        maxit = control$maxit,
        trace = as.integer(control$trace)
      ),
      control$nleqslv_control
    )
  )
}


#' Check the result returned by nleqslv
#'
#' Inspects the termination code (`termcd`) from `nleqslv::nleqslv()` and
#' either returns silently on success, issues a warning for partial
#' convergence, or stops with an informative error message on failure.
#'
#' @param root The list returned by `nleqslv::nleqslv()`.
#' @param label Character string used as a prefix in messages to identify
#'   the calling method.
#' @param ftol The function-value convergence tolerance, used to assess
#'   whether partial convergence is acceptable.
#'
#' @return `invisible(TRUE)` on success or acceptable convergence;
#'   otherwise stops or warns.
#'
#' @keywords internal
check_nleqslv_result <- function(root, label, ftol) {
  if (is.null(root$fvec)) {
    stop(
      sprintf("[%s] nleqslv failed: result does not contain `fvec`.", label),
      call. = FALSE
    )
  }

  if (is.null(root$termcd)) {
    stop(
      sprintf("[%s] nleqslv failed: result does not contain `termcd`.", label),
      call. = FALSE
    )
  }

  if (is.null(root$message)) {
    root$message <- ""
  }

  fmax <- max(abs(root$fvec))

  if (!is.finite(fmax)) {
    stop(
      sprintf(
        "[%s] nleqslv failed: final estimating equations contain non-finite values.",
        label
      ),
      call. = FALSE
    )
  }

  termcd <- as.integer(root$termcd)

  if (termcd == 1L) {
    return(invisible(TRUE))
  }

  if (termcd == 2L) {
    if (fmax <= ftol) {
      return(invisible(TRUE))
    }

    warning(
      sprintf(
        paste0(
          "[%s] nleqslv returned termcd = 2: the coefficient updates became very small, ",
          "but the estimating equations are not sufficiently close to zero.\n",
          "max|fvec| = %.3e; ftol = %.3e.\n",
          "The coefficients and pseudo-weights are returned, but users should decide ",
          "whether this remaining equation imbalance is acceptable."
        ),
        label, fmax, ftol
      ),
      call. = FALSE
    )

    return(invisible(TRUE))
  }

  if (termcd == 3L) {
    if (fmax <= ftol) {
      warning(
        sprintf(
          paste0(
            "[%s] nleqslv returned termcd = 3: no better point was found, ",
            "but the estimating equations are acceptably small.\n",
            "max|fvec| = %.3e; ftol = %.3e."
          ),
          label, fmax, ftol
        ),
        call. = FALSE
      )
      return(invisible(TRUE))
    }

    stop(
      sprintf(
        paste0(
          "[%s] nleqslv failed: no better point was found.\n",
          "termcd = 3; message = %s; max|fvec| = %.3e.\n",
          "The algorithm stopped before the estimating equations became small enough. ",
          "Try changing the nleqslv global strategy, ",
          "using xscalm = 'auto', simplifying p_formula, or combining rare factor levels."
        ),
        label, root$message, fmax
      ),
      call. = FALSE
    )
  }

  if (termcd == 4L) {
    stop(
      sprintf(
        paste0(
          "[%s] nleqslv failed: iteration limit was reached.\n",
          "termcd = 4; message = %s; max|fvec| = %.3e.\n",
          "Try increasing the maximum number of iterations, ",
          "changing the nleqslv global strategy, or simplifying p_formula."
        ),
        label, root$message, fmax
      ),
      call. = FALSE
    )
  }

  if (termcd %in% c(5L, 6L, 7L)) {
    jac_reason <- switch(
      as.character(termcd),
      "5" = "Jacobian is too ill-conditioned.",
      "6" = "Jacobian is singular.",
      "7" = "Jacobian is unusable."
    )

    stop(
      sprintf(
        paste0(
          "[%s] nleqslv failed: %s\n",
          "termcd = %s; message = %s; max|fvec| = %.3e.\n",
          "This means the estimating system is not numerically stable at the final iterate.\n",
          "Common reasons:\n",
          "  1. Some variables in p_formula are highly correlated (collinear).\n",
          "  2. Some categories are rare or absent in one of the samples.\n",
          "  3. There are too many variables or categories for the available data.\n",
          "  4. The variables almost perfectly distinguish the nonprobability sample ",
          "from the reference survey.\n",
          "Try simplifying p_formula, removing overlapping variables, combining rare factor levels, ",
          "or changing the global strategy in pw_solver_control()."
        ),
        label, jac_reason, termcd, root$message, fmax
      ),
      call. = FALSE
    )
  }

  if (termcd == -10L) {
    stop(
      sprintf(
        paste0(
          "[%s] nleqslv solver failed due to an internal numerical issue.\n",
          "termcd = -10; message = %s; max|fvec| = %.3e.\n",
          "This is likely caused by numerical instability in the model.\n",
          "Try simplifying p_formula, removing highly correlated variables, ",
          "or combining rare factor levels."
        ),
        label, root$message, fmax
      ),
      call. = FALSE
    )
  }

  stop(
    sprintf(
      paste0(
        "[%s] nleqslv failed with unrecognized termination code.\n",
        "termcd = %s; message = %s; max|fvec| = %.3e."
      ),
      label, termcd, root$message, fmax
    ),
    call. = FALSE
  )
}

#' Solve the participation model estimating equations
#'
#' A unified wrapper that calls `nleqslv::nleqslv()` to solve the
#' system of estimating equations \eqn{g(\beta) = 0}, then validates the
#' result via `check_nleqslv_result()`.
#'
#' @param beta_start Numeric vector of starting values for the coefficient
#'   vector \eqn{\beta}.
#' @param fn Function returning the estimating equation vector \eqn{g(\beta)}.
#' @param jac Function returning the Jacobian matrix \eqn{J(\beta)}.
#' @param label Character string used as a prefix in error messages to
#'   identify the calling method.
#' @param control A list created by `pw_solver_control()`.
#' @param ... Additional arguments passed to `fn` and `jac`
#'   (e.g., design matrices and weights).
#'
#' @return A list with components `coefficients`, `iterations`, `solver`,
#'   `solver_result`, `solver_method`, `fvec`, `termcd`, and `message`.
#'
#' @keywords internal
solve_participation_model <- function(
    beta_start,
    fn,
    jac,
    label,
    control = NULL,
    ...
) {

  if (!is.list(control) || is.null(control$solver)) {
    stop(
      "`control` must be created by pw_solver_control().",
      call. = FALSE
    )
  }

  if (tolower(control$solver) != "nleqslv") {
    stop(
      "Only solver = 'nleqslv' is currently supported.",
      call. = FALSE
    )
  }

  if (!is.numeric(beta_start) || any(!is.finite(beta_start))) {
    stop(
      "`beta_start` must be a finite numeric vector.",
      call. = FALSE
    )
  }

  if (!is.function(fn)) {
    stop("`fn` must be a function.", call. = FALSE)
  }

  if (!is.function(jac)) {
    stop("`jac` must be a function.", call. = FALSE)
  }

  args <- prepare_nleqslv_args(control)

  root <- do.call(
    nleqslv::nleqslv,
    c(
      list(
        x = beta_start,
        fn = fn,
        jac = jac
      ),
      args,
      list(...)
    )
  )

  check_nleqslv_result(
    root = root,
    label = label,
    ftol = args$control$ftol
  )

  list(
    coefficients = as.numeric(root$x),
    iterations = root$iter,
    solver = "nleqslv",
    solver_result = root,
    solver_method = args$method,
    fvec = root$fvec,
    termcd = root$termcd,
    message = root$message
  )
}
