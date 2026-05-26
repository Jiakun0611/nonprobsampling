est_pw <- function(
    data,
    sp_order = c("size", "given"),
    precali = TRUE,
    p_formula = NULL,
    method = NULL,
    na.action = stats::na.omit,
    sc_wname = "pseudo_wts",
    control = pw_solver_control(),
    verbose = FALSE
) {

  sp_order <- match.arg(sp_order)

  if (!is.null(method)) {
    method <- tolower(trimws(method))
    if (method == "cali") method <- "calibration"
  }

  #--------------------------------------------------------------------------#
  # Step 1. Parse input and basic validation
  #--------------------------------------------------------------------------#
  step1 <- tryCatch({

    check_ipwm_inputs_build(
      data      = data,
      p_formula = p_formula,
      method    = method,
      sp_order  = sp_order,
      precali   = precali,
      sc_wname  = sc_wname,
      verbose   = verbose
    )

    parsed <- parse_ipwm_data(data)

    sc     <- parsed$sc
    sp_des <- parsed$sp_des
    sp     <- parsed$sp_vars
    n_ref  <- parsed$n_ref

    if (n_ref == 1L) {
      sp     <- sp[[1]]
      sp_des <- sp_des[[1]]
    }

    weight <- rep("sp_wts", n_ref)

    method <- if (n_ref > 1L) {
      "multi"
    } else if (is.null(method)) {
      "calibration"
    } else {
      method
    }

    list(
      sc     = sc,
      sp_des = sp_des,
      sp     = sp,
      n_ref  = n_ref,
      weight = weight,
      method = method
    )

  }, error = function(e) {
    stop("Step 1 (input parsing and validation) failed: ", e$message, call. = TRUE)
  })

  sc     <- step1$sc
  sp_des <- step1$sp_des
  sp     <- step1$sp
  n_ref  <- step1$n_ref
  weight <- step1$weight
  method <- step1$method

  sc0 <- sc

  #--------------------------------------------------------------------------#
  # Step 2. Auto-build and preprocess p_formula
  #--------------------------------------------------------------------------#
  step2 <- tryCatch({

    log_messages <- character(0)

    if (is.null(p_formula)) {
      built <- p_formula_construction(
        sc     = sc,
        sp     = sp,
        weight = weight
      )

      p_formula    <- built$p_formula
      log_messages <- c(log_messages, built$log_messages)
    }

    list(
      p_formula    = p_formula,
      log_messages = log_messages
    )

  }, error = function(e) {
    stop("Step 2 (p_formula construction) failed: ", e$message, call. = TRUE)
  })

  p_formula    <- step2$p_formula
  log_messages <- step2$log_messages

  #--------------------------------------------------------------------------#
  # Step 3. NA processing
  #--------------------------------------------------------------------------#
  step3 <- tryCatch({

    process_na_build(
      sc        = sc,
      sp        = sp,
      sp_des    = sp_des,
      p_formula = p_formula,
      na.action = na.action,
      n_ref     = n_ref,
      verbose   = verbose
    )

  }, error = function(e) {
    stop("Step 3 (NA processing) failed: ", e$message, call. = TRUE)
  })

  na_mode       <- step3$na_mode
  sc            <- step3$sc
  sp            <- step3$sp
  sp_des        <- step3$sp_des
  keep_sc       <- step3$keep_sc
  keep_sp       <- step3$keep_sp
  n_sp_orig     <- step3$n_sp_orig
  na_action_obj <- step3$na_action_obj
  na_summary    <- step3$na_summary

  #--------------------------------------------------------------------------#
  # Step 4. Process p_formula
  #--------------------------------------------------------------------------#
  step4 <- tryCatch({

     processed <- process_p_formula(
      sc              = sc,
      sp              = sp,
      weight          = weight,
      Pre.calibration = precali,
      p_formula       = p_formula,
      sp_order        = sp_order,
      verbose         = verbose
    )

    list(
      sc           = processed$sc,
      sp           = processed$sp,
      p_vars       = processed$vars,
      log_messages = processed$log_messages
    )

  }, error = function(e) {
    stop("Step 4 (process p_formula) failed: ", e$message, call. = TRUE)
  })

  sc     <- step4$sc
  sp     <- step4$sp
  p_vars <- step4$p_vars

  if (!is.null(step4$log_messages)) {
    log_messages <- c(log_messages, step4$log_messages)
  }

  #--------------------------------------------------------------------------#
  # Step 5. Estimate pseudo-weights
  #--------------------------------------------------------------------------#
  result <- tryCatch({

    if (method %in% c("alp", "clw", "calibration")) {

      ipwm_one_build(
        sc           = sc,
        sp           = sp,
        sp_des       = sp_des,
        vars         = p_vars,
        weight       = weight,
        method       = method,
        control      = control,
        verbose      = verbose,
        log_messages = log_messages
      )

    } else if (method == "multi") {

      out <- ipwm_multi_build(
        sc           = sc,
        sp           = sp,
        sp_des       = sp_des,
        vars         = p_vars,
        weight       = weight,
        sp_order     = sp_order,
        control      = control,
        verbose      = verbose,
        log_messages = log_messages
      )

      out$method <- "multi"
      out

    } else {
      stop("Unknown method: ", method, call. = FALSE)
    }

  }, error = function(e) {
    stop("Step 5 (pseudo-weight estimation) failed: ", e$message, call. = TRUE)
  })

  #--------------------------------------------------------------------------#
  # Step 6. Construct sc_updated based on na_mode
  #--------------------------------------------------------------------------#
  sc_out <- tryCatch({

    reconstruct_sc_output(
      sc0           = sc0,
      w_fit         = result$pseudo_weights,
      keep_sc       = keep_sc,
      na_mode       = na_mode,
      na_action_obj = na_action_obj,
      sc_wname      = sc_wname
    )

  }, error = function(e) {
    stop("Step 6 (reconstruct sc_updated) failed: ", e$message, call. = TRUE)
  })

  #--------------------------------------------------------------------------#
  # Step 7. Finalize and return
  #--------------------------------------------------------------------------#
  result <- tryCatch({

    result <- finalize_pw_fit(
      result        = result,
      sc_out        = sc_out,
      sc0           = sc0,
      sc_wname      = sc_wname,
      na_mode       = na_mode,
      keep_sc       = keep_sc,
      na_action_obj = na_action_obj
    )

    result$call              <- match.call()
    result["na_summary"]     <- list(na_summary)

    result

  }, error = function(e) {
    stop("Step 7 (finalize output) failed: ", e$message, call. = TRUE)
  })

  invisible(result)
}
