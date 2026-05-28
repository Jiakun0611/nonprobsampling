#--------------------------------------------------#
# Helper: build a default one-sided p_formula from shared variables
#--------------------------------------------------#
build_one_formula <- function(sc, sp_i, weight_i, sp_name) {
  log_messages <- character(0)

  shared     <- intersect(colnames(sc), colnames(sp_i))
  drop_these <- weight_i
  vars       <- setdiff(shared, drop_these)

  if (length(vars) == 0L) {
    stop(
      sprintf("No shared covariates found to build default p_formula for %s.", sp_name),
      call. = FALSE
    )
  }

  fml <- as.formula(paste("~", paste(vars, collapse = " + ")))

  msg <- paste0(
    "Generated default p_formula for ", sp_name, ": ",
    paste(deparse(fml), collapse = ""),
    "\n"
  )

  log_messages <- c(log_messages, msg)

  return(list(
    p_formula    = fml,
    log_messages = log_messages
  ))
}
