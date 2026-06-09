#' Build the domains summary data frame
#'
#' Constructs the `domains` data frame that is stored in a `pwmean` object.
#' Each row corresponds to one domain. The 95% confidence intervals are
#' computed using the 0.975 normal quantile (approximately 1.96).
#'
#' @param labels Character vector of domain labels, one entry per domain.
#' @param mean_unw Numeric vector of unweighted (naive) domain means.
#' @param se_unw Numeric vector of standard errors for the unweighted means.
#' @param mean_adj Numeric vector of pseudo-weighted (adjusted) domain means.
#' @param se_adj Numeric vector of standard errors for the adjusted means.
#'
#' @return A data frame with one row per domain and columns `domain`,
#'   `unweighted_mean`, `unweighted_se`, `unweighted_lower`,
#'   `unweighted_upper`, `adjusted_mean`, `adjusted_se`, `adjusted_lower`,
#'   and `adjusted_upper`.
#'
#' @keywords internal
build_domains_df <- function(labels, mean_unw, se_unw, mean_adj, se_adj) {
  z975 <- stats::qnorm(0.975)
  df <- data.frame(
    domain          = labels,
    unweighted_mean = mean_unw,
    unweighted_se   = se_unw,
    adjusted_mean   = mean_adj,
    adjusted_se     = se_adj,
    stringsAsFactors = FALSE
  )
  df$unweighted_lower <- df$unweighted_mean - z975 * df$unweighted_se
  df$unweighted_upper <- df$unweighted_mean + z975 * df$unweighted_se
  df$adjusted_lower   <- df$adjusted_mean   - z975 * df$adjusted_se
  df$adjusted_upper   <- df$adjusted_mean   + z975 * df$adjusted_se
  df[, c(
    "domain",
    "unweighted_mean", "unweighted_se", "unweighted_lower", "unweighted_upper",
    "adjusted_mean",   "adjusted_se",   "adjusted_lower",   "adjusted_upper"
  )]
}


#' Assemble the final output object
#'
#' Combines the pseudo-weighted estimates and naive (unweighted) estimates
#' into a plain list to be returned by `pwmean()`.
#'
#' @param build A `pw_fit` object returned by the build step.
#' @param est A list returned by `dispatch_estimator()`, with components
#'   `type`, `labels`, and `estimates`.
#' @param naive A list returned by `naive_mean()`, with the same structure
#'   as `est`.
#' @param na_info NA-handling information returned by `process_na_yz()`.
#'
#' @return A list with components `method`, `domains`, and `na`.
#'   The S3 class is assigned by the calling function `pwmean()`.
#'
#' @keywords internal
assemble_output <- function(build, est, naive, na_info) {

  #----------------------------------#
  # Step 1: basic checks
  #----------------------------------#
  if (is.null(est$type) || is.null(naive$type)) {
    stop("`est` and `naive` must both contain a `type` field.", call. = FALSE)
  }

  if (!identical(est$type, naive$type)) {
    stop("`est$type` and `naive$type` do not match.", call. = FALSE)
  }

  #----------------------------------#
  # Step 2: single-result case
  #----------------------------------#
  if (identical(est$type, "single")) {

    domains_df <- build_domains_df(
      labels   = naive$labels,
      mean_unw = naive$estimates$mean,
      se_unw   = sqrt(naive$estimates$variance),
      mean_adj = est$estimates$mean,
      se_adj   = sqrt(est$estimates$variance)
    )

    return(list(method = build$method, domains = domains_df, na.action = na_info$na_action))
  }

  #----------------------------------#
  # Step 3: multi-result case
  #----------------------------------#
  if (identical(est$type, "multi")) {

    if (length(est$labels) != length(est$estimates)) {
      stop("Length mismatch between `est$labels` and `est$estimates`.", call. = FALSE)
    }

    if (length(naive$labels) != length(naive$estimates)) {
      stop("Length mismatch between `naive$labels` and `naive$estimates`.", call. = FALSE)
    }

    if (!identical(est$labels, naive$labels)) {
      stop("`est$labels` and `naive$labels` do not match.", call. = FALSE)
    }

    domains_df <- build_domains_df(
      labels   = est$labels,
      mean_unw = vapply(naive$estimates, function(x) x$mean,            numeric(1)),
      se_unw   = vapply(naive$estimates, function(x) sqrt(x$variance),  numeric(1)),
      mean_adj = vapply(est$estimates,   function(x) x$mean,            numeric(1)),
      se_adj   = vapply(est$estimates,   function(x) sqrt(x$variance),  numeric(1))
    )

    return(list(method = build$method, domains = domains_df, na.action = na_info$na_action))
  }

  #----------------------------------#
  # Step 4: unsupported type
  #----------------------------------#
  stop("Unsupported result type in `est$type` / `naive$type`.", call. = FALSE)
}
