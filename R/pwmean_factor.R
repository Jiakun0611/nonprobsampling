# Estimates prevalence for each level of a factor outcome.
pwmean_factor <- function(
    object,
    y,
    zcol = NULL,
    na.action = stats::na.omit,
    mc = match.call()
) {

  y_factor <- droplevels(object$internal$raw_sc[[y]])
  y_levels <- levels(y_factor)

  if (length(y_levels) < 1L) {
    stop(
      sprintf("Outcome variable '%s' must contain at least one non-missing level.", y),
      call. = FALSE
    )
  }

  level_outputs <- lapply(seq_along(y_levels), function(j) {
    pwmean_one_factor_level(
      object = object,
      y = y,
      level = y_levels[[j]],
      level_index = j,
      zcol = zcol,
      na.action = na.action,
      mc = mc
    )
  })

  result <- level_outputs[[1L]]
  result$estimates <- do.call(rbind, lapply(level_outputs, function(x) x$estimates))
  row.names(result$estimates) <- NULL
  result$call <- mc
  class(result) <- c("pwmean_factor", "pwmean")

  result
}

# Helper: convert one factor level to 0/1 and estimates its prevalence.
pwmean_one_factor_level <- function(
    object,
    y,
    level,
    level_index,
    zcol = NULL,
    na.action = stats::na.omit,
    mc = match.call()
) {

  tmp_object <- object
  tmp_y <- make_factor_outcome_name(
    y = y,
    level_index = level_index,
    existing_names = names(tmp_object$internal$raw_sc)
  )

  tmp_object$internal$raw_sc[[tmp_y]] <-
    as.numeric(tmp_object$internal$raw_sc[[y]] == level)

  out <- pwmean_numeric(
    object = tmp_object,
    y = tmp_y,
    zcol = zcol,
    na.action = na.action,
    mc = mc,
    check_inputs = FALSE
  )

  out$estimates <- add_factor_outcome_columns(
    estimates = out$estimates,
    y = y,
    level = level,
    has_domain = !is.null(zcol)
  )

  out
}

# Helper: creates a temporary internal outcome name that does not overwrite user data.
make_factor_outcome_name <- function(y, level_index, existing_names) {
  base_name <- paste0("..pwmean_", make.names(y), "_", level_index)
  out <- base_name
  suffix <- 1L

  while (out %in% existing_names) {
    suffix <- suffix + 1L
    out <- paste0(base_name, "_", suffix)
  }

  out
}

# Helper: adds user-facing category and domain columns for factor outcomes.
add_factor_outcome_columns <- function(estimates, y, level, has_domain) {
  category_label <- paste0(y, " = ", level)
  domain_labels <- estimates$domain

  if (!isTRUE(has_domain)) {
    domain_labels <- rep("Overall", nrow(estimates))
  }

  estimates$category <- rep(category_label, nrow(estimates))
  estimates$domain <- domain_labels
  estimates[, c(
    "category", "domain",
    setdiff(names(estimates), c("category", "domain"))
  )]
}
