#--------------------------------------------------#
# Helper: build a default one-sided p_formula from shared variables
#--------------------------------------------------#
build_one_formula <- function(sc, sp_i, weight_i, sp_name = NULL) {
  log_messages <- character(0)

  shared     <- intersect(colnames(sc), colnames(sp_i))
  drop_these <- weight_i
  vars       <- setdiff(shared, drop_these)

  if (length(vars) == 0L) {
    stop(
      if (is.null(sp_name)) {
        "No shared covariates found to build default p_formula."
      } else {
        sprintf("No shared covariates found to build default p_formula for %s.", sp_name)
      },
      call. = FALSE
    )
  }

  fml <- stats::as.formula(paste("~", paste(vars, collapse = " + ")))

  label <- if (is.null(sp_name)) "Generated default p_formula" else paste0("Generated default p_formula for ", sp_name)

  msg <- paste0(
    label, ": ",
    paste(deparse(fml), collapse = ""),
    "\n"
  )

  log_messages <- c(log_messages, msg)

  return(list(
    p_formula    = fml,
    log_messages = log_messages
  ))
}
