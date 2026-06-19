# Runs the pwmean estimation pipeline for a numeric outcome.
pwmean_numeric <- function(
    object,
    y,
    zcol = NULL,
    na.action = stats::na.omit,
    mc = match.call(),
    check_inputs = TRUE
) {

  #------------------------------#
  # Step 1: input checking
  #------------------------------#
  if (isTRUE(check_inputs)) {
    tryCatch(
      check_ipwm_inputs_estimate(object, y, zcol),
      error = function(e)
        stop("Step 1 (input check) failed: ", e$message, call. = FALSE)
    )
  }

  #------------------------------#
  # Step 2: prepare sc data
  #------------------------------#
  sc_data <- tryCatch(
    prepare_sc_data(object),
    error = function(e)
      stop("Step 2 (prepare_sc_data) failed: ", e$message, call. = FALSE)
  )

  #------------------------------#
  # Step 3: NA processing
  #------------------------------#
  yz_data <- tryCatch(
    process_na_yz(
      sc_data = sc_data,
      y = y,
      zcol = zcol,
      na.action = na.action
    ),
    error = function(e)
      stop("Step 3 (process_na_yz) failed: ", e$message, call. = FALSE)
  )

  #------------------------------#
  # Step 4: dispatch estimator
  #------------------------------#
  est <- tryCatch(
    dispatch_estimator(
      build = object,
      yz_data = yz_data
    ),
    error = function(e)
      stop("Step 4 (dispatch_estimator) failed: ", e$message, call. = FALSE)
  )

  #------------------------------#
  # Step 5: naive estimator
  #------------------------------#
  naive <- tryCatch(
    naive_mean(
      object$internal$raw_sc,
      domain_var = zcol,
      y = y
    ),
    error = function(e)
      stop("Step 5 (naive_mean) failed: ", e$message, call. = FALSE)
  )

  #------------------------------#
  # Step 6: assemble output
  #------------------------------#
  result <- tryCatch(
    assemble_output(
      build = object,
      est = est,
      naive = naive,
      na_info = yz_data$na_info
    ),
    error = function(e)
      stop("Step 6 (assemble_output) failed: ", e$message, call. = FALSE)
  )

  stopifnot(is.list(result))

  result$call <- mc

  class(result) <- "pwmean"

  return(result)
}
