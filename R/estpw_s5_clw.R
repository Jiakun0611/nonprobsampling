#' Build pseudo-weights using the CLW method
#'
#' @param vars Character vector of predictor variable names.
#' @param sc Data frame. The nonprobability sample.
#' @param sp Data frame. The probability reference sample.
#' @param sp_des A `survey.design2` or `svyrep.design` object for `sp`.
#' @param wts.col Character scalar. Name of the survey-weight column in `sp`.
#' @param control A list created by `pw_solver_control()`.
#' @param verbose Logical. If `TRUE`, convergence messages are printed.
#' @param log_messages Character vector of messages accumulated in earlier
#'   steps, appended to the returned object unchanged.
#'
#' @return A list with components:
#'   \item{weights}{Numeric vector of CLW pseudo-weights for `sc`.}
#'   \item{coefficients}{Named numeric vector of participation model coefficients.}
#'   \item{solver_diagnostics}{List of solver convergence diagnostics.}
#'   \item{log_messages}{Updated character vector of log messages.}
#'   \item{internal}{List containing design matrices (`Xc`, `Xp`), fitted
#'     participation probabilities (`pi_sc`, `pi_sp`), and sandwich components
#'     (`S_beta`, `D`) needed by the estimation stage.}
#'
#' @keywords internal
clw_build <- function(vars, sc, sp, sp_des, wts.col,
                      control,
                      verbose = FALSE,
                      log_messages = NULL) {

  if (is.null(log_messages)) {
    log_messages <- character(0)
  }

  Xc <- add_intercept(vars = vars, data = sc)
  Xp <- add_intercept(vars = vars, data = sp)

  check_design_identifiability(Xc, label = "One_Ref_CLW")
  check_design_identifiability(Xp, label = "One_Ref_CLW")

  if (ncol(Xc) != ncol(Xp)) {
    stop(
      "CLW failed: Xc and Xp must have the same number of columns.",
      call. = FALSE
    )
  }

  if (!wts.col %in% names(sp)) {
    stop(
      sprintf("CLW failed: weight column '%s' was not found in `sp`.", wts.col),
      call. = FALSE
    )
  }

  wts_sp <- sp[[wts.col]]

  if (!is.numeric(wts_sp) || any(!is.finite(wts_sp)) || any(wts_sp <= 0)) {
    stop(
      "CLW failed: reference survey weights must be positive finite numbers.",
      call. = FALSE
    )
  }

  if (verbose) {
    message(sprintf("Fitting CLW participation model with %d variables.", ncol(Xc)))
  }

  beta_start <- clw_start(
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp
  )

  sol <- solve_participation_model(
    beta_start = beta_start,
    fn = clw_fn,
    jac = clw_jac,
    label = "One_Ref_CLW",
    control = control,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp
  )

  beta <- sol$coefficients

  eta_sc <- as.vector(Xc %*% beta)
  eta_sp <- as.vector(Xp %*% beta)

  if (any(!is.finite(eta_sc)) || any(!is.finite(eta_sp))) {
    stop(
      "CLW failed: final linear predictor contains non-finite values.",
      call. = FALSE
    )
  }

  pi_sc <- as.vector(expit(eta_sc))
  pi_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(pi_sc)) || any(!is.finite(pi_sp))) {
    stop(
      "CLW failed: final fitted probabilities contain non-finite values.",
      call. = FALSE
    )
  }

  if (any(pi_sc <= 0 | pi_sc >= 1)) {
    stop(
      "CLW failed: final fitted probabilities in sc must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  wts_sc <- 1 / pi_sc

  if (any(!is.finite(wts_sc))) {
    stop(
      "CLW failed: final pseudo-weights contain non-finite values.",
      call. = FALSE
    )
  }

  f_final <- clw_fn(
    beta = beta,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp
  )

  max_abs_eq <- max(abs(f_final))

  if (!is.finite(max_abs_eq)) {
    stop(
      "CLW failed: final estimating equations contain non-finite values.",
      call. = FALSE
    )
  }

  if (verbose) {
    if (sol$termcd == 1L || sol$termcd == 2L) {
      message(sprintf("Converged after %d iterations (max |eq| = %.3e).",
                      sol$iterations, max_abs_eq))
    } else {
      message(sprintf(
        "WARNING: solver did not converge (termcd = %d). Check solver_diagnostics for details.",
        sol$termcd
      ))
    }
  }

  S_beta_full <- t(wts_sp * pi_sp * (1 - pi_sp) * Xp) %*% Xp
  D <- compute_D_CLW(sp_des, pi_sp, Xp)

  out <- list(
    weights      = wts_sc,
    coefficients = setNames(as.numeric(beta), colnames(Xc)),

    solver_diagnostics = list(
      solver  = sol$solver,
      termcd  = sol$termcd,
      message = sol$message,
      method  = sol$solver_method,
      iter    = sol$iterations,
      fmax    = max_abs_eq
    ),

    log_messages = log_messages,
    internal = list(
      Xc     = Xc,
      Xp     = Xp,
      pi_sc  = pi_sc,
      pi_sp  = pi_sp,
      S_beta = S_beta_full,
      D      = D
    )
  )

  return(out)
}
