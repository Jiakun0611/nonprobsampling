#' CLW estimating equations
#' @noRd
clw_fn <- function(beta, Xc, Xp, wts_sp, ...) {
  eta_sp <- as.vector(Xp %*% beta)

  if (any(!is.finite(eta_sp))) {
    stop(
      "CLW failed: reference-sample linear predictor became non-finite.",
      call. = FALSE
    )
  }

  pi_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(pi_sp))) {
    stop(
      paste(
        "CLW failed: fitted probabilities became non-finite.",
        "This usually indicates numerical overflow or unstable coefficient values."
      ),
      call. = FALSE
    )
  }

  colSums(Xc) - colSums(wts_sp * pi_sp * Xp)
}


#' Jacobian of the CLW estimating equations
#' @noRd
clw_jac <- function(beta, Xc, Xp, wts_sp,
                    label = "One_Ref_CLW",
                    tol_singular = 1e-7,
                    ...) {
  eta_sp <- as.vector(Xp %*% beta)

  if (any(!is.finite(eta_sp))) {
    stop(
      sprintf("[%s] CLW failed: reference-sample linear predictor became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  pi_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(pi_sp))) {
    stop(
      sprintf(
        paste(
          "[%s] CLW failed: fitted probabilities became non-finite when computing Jacobian.",
          "This usually indicates numerical overflow or unstable coefficient values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  g <- colSums(Xc) - colSums(wts_sp * pi_sp * Xp)

  J <- -t(wts_sp * pi_sp * (1 - pi_sp) * Xp) %*% Xp

  check_estimating_system(
    J = J,
    g = g,
    label = label,
    tol_singular = tol_singular
  )

  J
}


#' Starting values for the coefficients of the CLW participation model
#' @noRd
clw_start <- function(Xc, Xp, wts_sp) {
  p_dim <- ncol(Xc)
  beta_start <- rep(0, p_dim)

  pi_sp0 <- as.vector(expit(Xp %*% beta_start))

  if (any(!is.finite(pi_sp0))) {
    stop(
      "CLW failed: cannot construct initial values because initial fitted probabilities are non-finite.",
      call. = FALSE
    )
  }

  f_p0 <- colSums(wts_sp * pi_sp0 * Xp)
  f_c0 <- colSums(Xc)

  if (!is.finite(f_p0[1]) || !is.finite(f_c0[1]) ||
      f_p0[1] <= 0 || f_c0[1] <= 0) {
    stop(
      "CLW failed: cannot construct initial intercept because intercept totals are invalid.",
      call. = FALSE
    )
  }

  beta_start[1] <- -log(f_p0[1] / f_c0[1])

  if (any(!is.finite(beta_start))) {
    stop(
      "CLW failed: initial coefficient vector contains non-finite values.",
      call. = FALSE
    )
  }

  beta_start
}

