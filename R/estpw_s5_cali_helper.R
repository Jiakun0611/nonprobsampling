#' Calibration estimating equations
#' @keywords internal
raking_fn <- function(beta, Xc, f_p, ...) {
  eta <- as.vector(Xc %*% beta)

  if (any(!is.finite(eta))) {
    stop(
      "Calibration failed: linear predictor became non-finite.",
      call. = FALSE
    )
  }

  if (any(eta < -log(.Machine$double.xmax))) {
    stop(
      paste(
        "Calibration failed: exp(-eta) overflow.",
        "This indicates an unstable coefficient value."
      ),
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      "Calibration failed: pseudo-weights became non-finite.",
      call. = FALSE
    )
  }

  f_c <- colSums(wts_sc * Xc)

  as.numeric(f_c - f_p)
}

#' Jacobian of the calibration estimating equations
#' @keywords internal
raking_jac <- function(beta, Xc, f_p,
                       label = "One_Ref_Calibration",
                       tol_singular = 1e-7,
                       ...) {
  eta <- as.vector(Xc %*% beta)

  if (any(!is.finite(eta))) {
    stop(
      sprintf("[%s] Calibration failed: linear predictor became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  if (any(eta < -log(.Machine$double.xmax))) {
    stop(
      sprintf(
        paste(
          "[%s] Calibration failed: exp(-eta) overflow when computing Jacobian.",
          "This usually indicates an unstable coefficient value."
        ),
        label
      ),
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      sprintf("[%s] Calibration failed: pseudo-weights became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  g <- colSums(wts_sc * Xc) - f_p
  J <- -t(wts_sc * Xc) %*% Xc

  check_estimating_system(
    J = J,
    g = g,
    label = label,
    tol_singular = tol_singular
  )

  J
}

#' Starting values for the coefficients of the calibration participation model
#' @keywords internal
raking_start <- function(Xc, f_p) {
  p_dim <- ncol(Xc)
  beta_start <- rep(0, p_dim)

  wts0 <- as.vector(exp(-Xc %*% beta_start))
  f_c0 <- colSums(wts0 * Xc)

  if (!is.finite(f_p[1]) || !is.finite(f_c0[1]) || f_p[1] <= 0 || f_c0[1] <= 0) {
    stop(
      "Calibration failed: cannot construct initial intercept because intercept totals are invalid.",
      call. = FALSE
    )
  }

  beta_start[1] <- -log(f_p[1] / f_c0[1])

  if (any(!is.finite(beta_start))) {
    stop(
      "Calibration failed: initial coefficient vector contains non-finite values.",
      call. = FALSE
    )
  }

  beta_start
}
