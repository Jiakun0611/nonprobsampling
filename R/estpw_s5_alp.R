#' Build pseudo-weights using the ALP method
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
#'   \item{weights}{Numeric vector of ALP pseudo-weights for `sc`.}
#'   \item{coefficients}{Named numeric vector of participation model coefficients.}
#'   \item{solver_diagnostics}{List of solver convergence diagnostics.}
#'   \item{log_messages}{Updated character vector of log messages.}
#'   \item{internal}{List containing design matrices (`Xc`, `Xp`), fitted
#'     participation probabilities (`p_sc`, `p_sp`), and sandwich components
#'     (`S_beta`, `D`) needed by the estimation stage.}
#'
#' @importFrom stats setNames
#' @keywords internal
alp_build <- function(vars, sc, sp, sp_des, wts.col,
                      control,
                      verbose = FALSE,
                      log_messages = NULL) {

  if (is.null(log_messages)) {
    log_messages <- character(0)
  }

  Xc <- add_intercept(vars = vars, data = sc)
  Xp <- add_intercept(vars = vars, data = sp)

  check_design_identifiability(Xc, label = "One_Ref_ALP")
  check_design_identifiability(Xp, label = "One_Ref_ALP")

  if (ncol(Xc) != ncol(Xp)) {
    stop(
      "ALP failed: Xc and Xp must have the same number of columns.",
      call. = FALSE
    )
  }

  if (!wts.col %in% names(sp)) {
    stop(
      sprintf("ALP failed: weight column '%s' was not found in `sp`.", wts.col),
      call. = FALSE
    )
  }

  wts_sp <- sp[[wts.col]]

  if (!is.numeric(wts_sp) || any(!is.finite(wts_sp)) || any(wts_sp <= 0)) {
    stop(
      "ALP failed: reference survey weights must be positive finite numbers.",
      call. = FALSE
    )
  }

  if (verbose) {
    message(sprintf("Fitting ALP participation model with %d variables.", ncol(Xc)))
  }

  beta_start <- alp_start(
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp
  )

  sol <- solve_participation_model(
    beta_start = beta_start,
    fn = alp_fn,
    jac = alp_jac,
    label = "One_Ref_ALP",
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
      "ALP failed: final linear predictor contains non-finite values.",
      call. = FALSE
    )
  }

  p_sc <- as.vector(expit(eta_sc))
  p_sp <- as.vector(expit(eta_sp))

  if (any(!is.finite(p_sc)) || any(!is.finite(p_sp))) {
    stop(
      "ALP failed: final fitted probabilities contain non-finite values.",
      call. = FALSE
    )
  }

  if (any(p_sc <= 0 | p_sc >= 1)) {
    stop(
      "ALP failed: final fitted probabilities in sc must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  pi_sc <- p_sc / (1 - p_sc)
  wts_sc <- 1 / pi_sc

  if (any(!is.finite(wts_sc))) {
    stop(
      "ALP failed: final pseudo-weights contain non-finite values.",
      call. = FALSE
    )
  }

  f_final <- alp_fn(
    beta = beta,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp
  )

  max_abs_eq <- max(abs(f_final))

  if (!is.finite(max_abs_eq)) {
    stop(
      "ALP failed: final estimating equations contain non-finite values.",
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

  S_beta_full <-
    t(p_sc * (1 - p_sc) * Xc) %*% Xc +
    t(wts_sp * p_sp * (1 - p_sp) * Xp) %*% Xp

  D <- compute_D_ALP(sp_des, p_sp, Xp)

  out <- list(
    weights      = wts_sc,
    coefficients = stats::setNames(as.numeric(beta), colnames(Xc)),

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
      p_sc   = p_sc,
      p_sp   = p_sp,
      S_beta = S_beta_full,
      D      = D
    )
  )

  return(out)
}
