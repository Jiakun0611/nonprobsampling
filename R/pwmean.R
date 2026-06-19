pwmean <- function(
    object,
    y,
    zcol = NULL,
    na.action = stats::na.omit
) {

  mc <- match.call()

  #------------------------------#
  # Step 1: input checking
  #------------------------------#
  tryCatch(
    check_ipwm_inputs_estimate(object, y, zcol),
    error = function(e)
      stop("Step 1 (input check) failed: ", e$message, call. = FALSE)
  )

  if (is.factor(object$internal$raw_sc[[y]])) {
    return(pwmean_factor(
      object = object,
      y = y,
      zcol = zcol,
      na.action = na.action,
      mc = mc
    ))
  }

  pwmean_numeric(
    object = object,
    y = y,
    zcol = zcol,
    na.action = na.action,
    mc = mc,
    check_inputs = FALSE
  )
}
