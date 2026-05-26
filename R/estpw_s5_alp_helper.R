#' ALP estimating equations
#' @keywords internal
alp_fn <- function(beta, Xc, Xp, wts_sp, ...) {
  eta_sc <- as.vector(Xc %*% beta)
  eta_sp <- as.vector(Xp %*% beta)

  if (any(!is.finite(eta_sc)) || any(!is.finite(eta_sp))) {
    stop(
      "ALP failed: linear predictor became non-finite.",
      call. = FALSE
    )
  }

  p_sc <- as.vector(expit(eta_sc))
  p_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(p_sc)) || any(!is.finite(p_sp))) {
    stop(
      paste(
        "ALP failed: fitted probabilities became non-finite.",
        "This usually indicates numerical overflow or unstable coefficient values."
      ),
      call. = FALSE
    )
  }

  colSums((1 - p_sc) * Xc) -
    colSums(wts_sp * p_sp * Xp)
}


#' Jacobian of the ALP estimating equations
#' @keywords internal
alp_jac <- function(beta, Xc, Xp, wts_sp,
                    label = "One_Ref_ALP",
                    tol_singular = 1e-7,
                    ...) {
  eta_sc <- as.vector(Xc %*% beta)
  eta_sp <- as.vector(Xp %*% beta)

  if (any(!is.finite(eta_sc)) || any(!is.finite(eta_sp))) {
    stop(
      sprintf("[%s] ALP failed: linear predictor became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  p_sc <- as.vector(expit(eta_sc))
  p_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(p_sc)) || any(!is.finite(p_sp))) {
    stop(
      sprintf(
        paste(
          "[%s] ALP failed: fitted probabilities became non-finite when computing Jacobian.",
          "This usually indicates numerical overflow or unstable coefficient values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  g <- colSums((1 - p_sc) * Xc) -
    colSums(wts_sp * p_sp * Xp)

  J <- -t(p_sc * (1 - p_sc) * Xc) %*% Xc -
    t(wts_sp * p_sp * (1 - p_sp) * Xp) %*% Xp

  check_estimating_system(
    J = J,
    g = g,
    label = label,
    tol_singular = tol_singular
  )

  J
}


#' Starting values for the coefficients of the ALP participation model
#' @keywords internal
alp_start <- function(Xc, Xp, wts_sp) {
  p_dim <- ncol(Xc)
  beta_start <- rep(0, p_dim)

  p_sp0 <- as.vector(expit(Xp %*% beta_start))
  p_sc0 <- as.vector(expit(Xc %*% beta_start))

  if (any(!is.finite(p_sp0)) || any(!is.finite(p_sc0))) {
    stop(
      "ALP failed: cannot construct initial values because initial fitted probabilities are non-finite.",
      call. = FALSE
    )
  }

  f_p0 <- colSums(wts_sp * p_sp0 * Xp)
  f_c0 <- colSums((1 - p_sc0) * Xc)

  if (!is.finite(f_p0[1]) || !is.finite(f_c0[1]) ||
      f_p0[1] <= 0 || f_c0[1] <= 0) {
    stop(
      "ALP failed: cannot construct initial intercept because intercept totals are invalid.",
      call. = FALSE
    )
  }

  beta_start[1] <- -log(f_p0[1] / f_c0[1])

  if (any(!is.finite(beta_start))) {
    stop(
      "ALP failed: initial coefficient vector contains non-finite values.",
      call. = FALSE
    )
  }

  beta_start
}
