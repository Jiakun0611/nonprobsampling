#' Build pseudo-weights using the calibration (raking) method
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
#'   \item{weights}{Numeric vector of calibration pseudo-weights for `sc`.}
#'   \item{coefficients}{Named numeric vector of participation model coefficients.}
#'   \item{solver_diagnostics}{List of solver convergence diagnostics.}
#'   \item{log_messages}{Updated character vector of log messages.}
#'   \item{internal}{List containing design matrices (`Xc`, `Xp`) and sandwich
#'     components (`S_beta`, `D`) needed by the estimation stage.}
#'
#' @keywords internal
raking_build <- function(vars, sc, sp, sp_des, wts.col,
                         control,
                         verbose = FALSE,
                         log_messages = NULL) {

  if (is.null(log_messages)) {
    log_messages <- character(0)
  }

  Xc <- add_intercept(vars = vars, data = sc)
  Xp <- add_intercept(vars = vars, data = sp)

  check_design_identifiability(Xc, label = "One_Ref_Calibration")
  check_design_identifiability(Xp, label = "One_Ref_Calibration")

  if (ncol(Xc) != ncol(Xp)) {
    stop(
      "Calibration failed: Xc and Xp must have the same number of columns.",
      call. = FALSE
    )
  }

  if (!wts.col %in% names(sp)) {
    stop(
      sprintf("Calibration failed: weight column '%s' was not found in `sp`.", wts.col),
      call. = FALSE
    )
  }

  wts_sp <- sp[[wts.col]]

  if (!is.numeric(wts_sp) || any(!is.finite(wts_sp)) || any(wts_sp <= 0)) {
    stop(
      "Calibration failed: reference survey weights must be positive finite numbers.",
      call. = FALSE
    )
  }

  f_p <- colSums(wts_sp * Xp)

  if (any(!is.finite(f_p))) {
    stop(
      "Calibration failed: reference weighted totals contain non-finite values.",
      call. = FALSE
    )
  }

  if (verbose) {
    message(sprintf("Fitting Calibration participation model with %d variables.", ncol(Xc)))
  }

  beta_start <- raking_start(Xc = Xc, f_p = f_p)

  sol <- solve_participation_model(
    beta_start = beta_start,
    fn = raking_fn,
    jac = raking_jac,
    label = "One_Ref_Calibration",
    control = control,
    Xc = Xc,
    f_p = f_p
  )

  beta <- sol$coefficients

  eta <- as.vector(Xc %*% beta)

  if (any(!is.finite(eta))) {
    stop(
      "Calibration failed: final linear predictor contains non-finite values.",
      call. = FALSE
    )
  }

  if (any(eta < -log(.Machine$double.xmax))) {
    stop(
      "Calibration failed: final pseudo-weights overflow.",
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      "Calibration failed: final pseudo-weights contain non-finite values.",
      call. = FALSE
    )
  }

  f_final <- raking_fn(beta, Xc = Xc, f_p = f_p)
  max_abs_eq <- max(abs(f_final))

  if (!is.finite(max_abs_eq)) {
    stop(
      "Calibration failed: final estimating equations contain non-finite values.",
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

  S_beta_full <- t(wts_sc * Xc) %*% Xc
  D <- compute_D_raking(sp_des, Xp)

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
      Xc = Xc,
      Xp = Xp,
      S_beta = S_beta_full,
      D = D
    )
  )

  return(out)
}
