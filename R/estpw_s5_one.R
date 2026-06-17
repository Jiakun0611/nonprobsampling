#' Dispatch one-reference pseudo-weight estimation
#'
#' @param sc Data frame. The nonprobability sample.
#' @param sp Data frame. The single probability reference sample (already
#'   NA-processed and p_formula-processed).
#' @param sp_des A `survey.design2` or `svyrep.design` object for `sp`.
#' @param vars Character vector of predictor variable names to use in the
#'   participation model design matrices.
#' @param weight Character scalar. Name of the survey-weight column in `sp`.
#' @param method Character scalar. One of `"alp"`, `"clw"`, `"calibration"`,
#'   or `"cali"`.
#' @param control A list created by `pw_solver_control()`.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param log_messages Character vector of messages accumulated in earlier
#'   steps, forwarded into the returned object.
#'
#' @return A list with components:
#'   \item{pseudo_weights}{Numeric vector of estimated pseudo-weights for `sc`.}
#'   \item{coefficients}{Named numeric vector of participation model coefficients.}
#'   \item{method}{Character scalar identifying the method used.}
#'   \item{solver_diagnostics}{List of solver convergence diagnostics.}
#'   \item{internal}{List of intermediate objects (design matrices, fitted
#'     probabilities, sandwich components) needed by the estimation stage.}
#'
#' @keywords internal
ipwm_one_build <- function(
    sc,
    sp,
    sp_des,
    vars = NULL,
    weight,
    method,
    control,
    verbose = FALSE,
    log_messages = NULL
) {

  # ---------------------------------------------------------------
  # Dispatch to *_build
  # ---------------------------------------------------------------
  m_lower <- tolower(method)

  if (m_lower == "alp") {

    built <- alp_build(
      sc = sc,
      sp = sp,
      sp_des = sp_des,
      vars = vars,
      wts.col = weight,
      control = control,
      verbose = verbose,
      log_messages = log_messages
    )

  } else if (m_lower == "clw") {

    built <- clw_build(
      sc = sc,
      sp = sp,
      sp_des = sp_des,
      vars = vars,
      wts.col = weight,
      control = control,
      verbose = verbose,
      log_messages = log_messages
    )

  } else if (m_lower == "calibration") {

    built <- raking_build(
      sc = sc,
      sp = sp,
      sp_des = sp_des,
      vars = vars,
      wts.col = weight,
      control = control,
      verbose = verbose,
      log_messages = log_messages
    )

  } else {
    stop("Unknown method. Use method = 'ALP', 'CLW', or 'calibration'.",
         call. = FALSE)
  }


  # ---------------------------------------------------------------
  # 4. Return object
  # ---------------------------------------------------------------
  internal_out <- built$internal
  internal_out[["log_messages"]] <- built$log_messages

  out <- list(
    pseudo_weights     = built$weights,
    coefficients       = built$coefficients,
    method             = method,
    solver_diagnostics = built$solver_diagnostics,
    internal           = internal_out
  )
  return(out)
}
