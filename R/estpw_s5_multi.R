#' Build pseudo-weights for the multi-reference calibration method
#'
#' @param sc Data frame. The nonprobability sample.
#' @param sp List of data frames. Each element is one probability reference
#'   sample (already NA-processed and p_formula-processed).
#' @param vars List of character vectors, one per reference sample, giving the
#'   predictor variable names shared with `sc`.
#' @param weight Character vector, one per reference sample, giving the
#'   survey-weight column name in each element of `sp`.
#' @param sp_des List of `survey.design2` or `svyrep.design` objects,
#'   one per reference sample.
#' @param sp_order Character scalar, either `"size"` (sort reference samples
#'   largest-first) or `"given"` (keep user order).
#' @param control A list created by `pw_solver_control()`.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param log_messages Character vector of messages accumulated in earlier
#'   steps, forwarded into the returned object.
#'
#' @return A list with components:
#'   \item{pseudo_weights}{Numeric vector of estimated pseudo-weights for `sc`.}
#'   \item{coefficients}{Named numeric vector of participation model coefficients.}
#'   \item{method}{Character scalar, always `"multi"`.}
#'   \item{solver_diagnostics}{List of solver convergence diagnostics.}
#'   \item{internal}{List of intermediate objects (block design matrices,
#'     reference weighted totals, block column indices, sandwich components,
#'     and sorted metadata) needed by the estimation stage.}
#'
#' @keywords internal
ipwm_multi_build <- function(
    sc, sp, vars, weight,
    sp_des, sp_order,
    control,
    verbose = FALSE,
    log_messages = NULL
) {

  if (is.null(log_messages)) {
    log_messages <- character(0)
  }

  step_try <- function(step_num, step_name, expr) {
    tryCatch(
      expr,
      error = function(e) {
        stop(
          sprintf(
            "Step %s (%s) failed: %s",
            step_num, step_name, conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }


  sorted <- step_try("1", "sort reference samples", {
    sort_by_sp_size(
      sp       = sp,
      vars     = vars,
      weight   = weight,
      design   = sp_des,
      sp_order = sp_order,
      verbose  = verbose
    )
  })

  sp_sorted     <- sorted$sp
  vars_sorted   <- sorted$vars
  weight_sorted <- sorted$weight
  design_sorted <- sorted$design
  log_messages  <- c(log_messages, sorted$log)

  valid <- step_try("2", "validate inputs", {
    check_input_multi(
      sc        = sc,
      sp_list   = sp_sorted,
      vars_list = vars_sorted,
      wts_cols  = weight_sorted,
      verbose   = verbose
    )
  })

  sc_work      <- valid$sc
  sp_list      <- valid$sp_list
  vars_XC      <- valid$vars_XC
  xcol         <- valid$xcol
  wts_cols     <- valid$wts_cols
  log_messages <- c(log_messages, valid$log)

  DM <- step_try("3", "construct design matrices", {
    Xc_Xp_Construction(
      vars_XC  = vars_XC,
      sc       = sc_work,
      sp_list  = sp_list,
      xcol     = xcol,
      wts_cols = wts_cols
    )
  })

  Xc       <- DM$Xc
  Xp_list  <- DM$Xp_list
  wts_list <- DM$wts_list

  if (verbose) {
    message(sprintf(
      "Fitting multi-reference calibration model: %d reference surveys, %d variables total.",
      length(Xp_list), ncol(Xc)
    ))
  }

  block_cols <- step_try("4", "match multi-reference blocks", {
    multi_raking_block_cols(
      Xc = Xc,
      Xp_list = Xp_list,
      label = "Multi_Calibration"
    )
  })

  f_p <- step_try("5", "construct reference weighted totals", {
    multi_raking_fp(
      Xp_list = Xp_list,
      wts_list = wts_list,
      label = "Multi_Calibration"
    )
  })

  beta_start <- step_try("6", "construct starting values", {
    multi_raking_start(
      Xc = Xc,
      Xp_list = Xp_list,
      block_cols = block_cols,
      f_p = f_p,
      label = "Multi_Calibration"
    )
  })

  sol <- step_try("7", "solve multi-reference calibration equations", {
    solve_participation_model(
      beta_start = beta_start,
      fn = multi_raking_fn,
      jac = multi_raking_jac,
      label = "Multi_Calibration",
      control = control,
      Xc = Xc,
      f_p = f_p,
      block_cols = block_cols
    )
  })

  beta <- sol$coefficients

  wts_sc <- step_try("8", "compute pseudo-weights", {
    eta <- as.vector(Xc %*% beta)

    if (any(!is.finite(eta))) {
      stop(
        "Multi-reference calibration failed: final linear predictor contains non-finite values.",
        call. = FALSE
      )
    }

    if (any(eta < -log(.Machine$double.xmax))) {
      stop(
        "Multi-reference calibration failed: final pseudo-weights overflow.",
        call. = FALSE
      )
    }

    out <- as.vector(exp(-eta))

    if (any(!is.finite(out))) {
      stop(
        "Multi-reference calibration failed: final pseudo-weights contain non-finite values.",
        call. = FALSE
      )
    }

    out
  })

  f_final <- step_try("9", "check final estimating equations", {
    out <- multi_raking_fn(
      beta = beta,
      Xc = Xc,
      f_p = f_p,
      block_cols = block_cols,
      label = "Multi_Calibration"
    )

    if (any(!is.finite(out))) {
      stop(
        "Multi-reference calibration failed: final estimating equations contain non-finite values.",
        call. = FALSE
      )
    }

    out
  })

  max_abs_eq <- max(abs(f_final))

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

  D <- step_try("10", "construct D matrix", {
    make_block_D_multi(
      sp_des_list = design_sorted,
      Xp_list     = Xp_list
    )
  })

  S_beta_full <- step_try("11", "construct S_beta", {
    t(wts_sc * Xc) %*% Xc
  })

  out <- list(
    pseudo_weights = wts_sc,

    coefficients   = setNames(as.numeric(beta), colnames(Xc)),

    method         = "multi",

    solver_diagnostics = list(
      solver  = sol$solver,
      termcd  = sol$termcd,
      message = sol$message,
      method  = sol$solver_method,
      iter    = sol$iterations,
      fmax    = max_abs_eq
    ),

    internal = list(
      Xc         = Xc,
      Xp_list    = Xp_list,
      wts_list   = wts_list,
      f_p        = f_p,
      block_cols = block_cols,
      D          = D,
      S_beta     = S_beta_full,

      xcol         = xcol,
      vars_XC      = vars_XC,
      wts_cols     = wts_cols,
      design       = design_sorted,
      sp_order     = sp_order,
      log_messages = log_messages
    )
  )

  return(out)
}
