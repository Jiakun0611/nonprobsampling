#' Estimate step for raking-ratio calibration
#'
#' Computes the domain-specific pseudo-weighted Hájek mean and its
#' Taylor-linearized variance for the raking-ratio calibration estimator.
#'
#' @param Y Outcome vector for the nonprobability sample.
#' @param Z Domain indicator vector. Use `rep(1, length(Y))` for the
#'   overall mean.
#' @param w Estimated pseudo-weights from the calibration build step.
#' @param X Calibration design matrix for the nonprobability sample.
#' @param D Design-based variance-covariance matrix of the estimated
#'   auxiliary totals from the reference survey or surveys. For multiple
#'   reference surveys, this is block diagonal.
#' @param S_beta Sensitivity matrix for the calibration estimating equations,
#'   typically \code{t(w * X) \%*\% X} for raking-ratio calibration.
#'
#' @return A list with components `mean` and `variance`.
#'
#' @keywords internal
raking_estimate <- function(Y, Z, w, X, D, S_beta) {

  # pseudo-weighted Hajek mean
  T1 <- sum((Y * Z) * w)
  T2 <- sum(Z * w)
  mu <- T1 / T2

  # linearization coefficient for beta estimation
  U_beta <- t((Y * Z) - mu * Z) %*% (w * X)
  b_vec  <- U_beta %*% qr.solve(S_beta)   # 1 x p

  # linearized residual after accounting for beta estimation
  resid <- (Y * Z) - mu * Z - as.vector(X %*% t(b_vec))

  # nonprobability sample component under the Poisson approximation
  v1 <- sum(w * (w - 1) * (resid^2))

  # reference-survey component from estimated auxiliary totals
  v2 <- b_vec %*% D %*% t(b_vec)

  variance <- as.vector((v1 + v2) / T2^2)

  list(
    mean = mu,
    variance = variance
  )
}
