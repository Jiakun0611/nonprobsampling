#' Compute the naive mean for one domain
#'
#' Computes the unweighted sample mean and its variance for observations
#' belonging to a single domain, identified by a 0/1 indicator vector.
#'
#' @param yvec Numeric or integer outcome vector of length \eqn{n}.
#' @param zvec Integer 0/1 domain indicator of length \eqn{n}, or NULL
#'   (treated as all-ones, i.e., the overall mean).
#'
#' @return A list with components:
#'
#' - `mean`: unweighted sample mean of `yvec` within the domain,
#'   or NA if the domain is empty.
#' - `variance`: estimated variance of the mean (\eqn{s^2/n}),
#'   or NA if fewer than two observations are available.
#'
#' @keywords internal
naive_mean_one_domain <- function(yvec, zvec = NULL) {

  if (!is.numeric(yvec)) {
    stop("`yvec` must be numeric.", call. = FALSE)
  }

  if (is.null(zvec)) {
    zvec <- rep.int(1L, length(yvec))
  }

  if (length(zvec) != length(yvec)) {
    stop("Length mismatch between `yvec` and `zvec`.", call. = FALSE)
  }

  keep <- (zvec == 1L)
  y_sub <- yvec[keep]
  n <- length(y_sub)

  out <- list(
    mean     = if (n > 0L) mean(y_sub) else NA_real_,
    variance = if (n > 1L) stats::var(y_sub) / n else NA_real_
  )

  out
}


#' Compute naive (unweighted) means from the convenience sample
#'
#' Filters the convenience sample to complete cases, then computes the
#' unweighted sample mean (and its variance) for each domain using
#' `naive_mean_one_domain()`. The domain structure is standardized via
#' `standardize_zcol()`.
#'
#' @param df A data frame containing the convenience sample
#'   (typically `build$internal$raw_sc`).
#' @param domain_var Single character string naming the domain variable in
#'   `df`, or NULL for the overall mean.
#' @param y Single character string naming the outcome variable in `df`.
#'
#' @return A list with components:
#'
#' - `type`: `"single"` for overall or binary domains;
#'   `"multi"` for factor/character domains.
#' - `labels`: character vector of domain labels.
#' - `estimates`: for `type = "single"`, a list with `mean` and `variance`.
#'   For `type = "multi"`, a list of such lists, one per domain level.
#'
#' @keywords internal
naive_mean <- function(df, domain_var = NULL, y) {

  #----------------------------------#
  # Step 1: basic checks
  #----------------------------------#
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(y) || length(y) != 1L || is.na(y) || !nzchar(y)) {
    stop("`y` must be a single non-empty character string.", call. = FALSE)
  }

  if (!y %in% names(df)) {
    stop(sprintf("Outcome variable '%s' not found in `df`.", y), call. = FALSE)
  }

  #----------------------------------#
  # Step 2: complete-case filtering for y and domain_var
  #----------------------------------#
  if (is.null(domain_var)) {
    ok <- !is.na(df[[y]])
  } else {
    if (!is.character(domain_var) || length(domain_var) != 1L ||
        is.na(domain_var) || !nzchar(domain_var)) {
      stop("`domain_var` must be NULL or a single non-empty character string.",
           call. = FALSE)
    }

    if (!domain_var %in% names(df)) {
      stop(sprintf("Domain variable '%s' not found in `df`.", domain_var),
           call. = FALSE)
    }

    ok <- !is.na(df[[y]]) & !is.na(df[[domain_var]])
  }

  df_cc <- df[ok, , drop = FALSE]
  yvec <- df_cc[[y]]

  #----------------------------------#
  # Step 3: standardize domain variable
  #----------------------------------#
  domain_info <- standardize_zcol(data = df_cc, zcol = domain_var)

  #----------------------------------#
  # Step 4: dispatch by domain mode
  #----------------------------------#
  if (identical(domain_info$mode, "overall")) {
    est <- naive_mean_one_domain(yvec = yvec, zvec = NULL)

    return(list(
      type = "single",
      labels = "Overall",
      estimates = est
    ))
  }

  if (identical(domain_info$mode, "binary")) {
    zvec <- domain_info$indicators[[1]]
    est <- naive_mean_one_domain(yvec = yvec, zvec = zvec)

    return(list(
      type = "single",
      labels = domain_info$labels,
      estimates = est
    ))
  }

  if (identical(domain_info$mode, "factor")) {
    est_list <- lapply(seq_along(domain_info$labels), function(j) {
      naive_mean_one_domain(
        yvec = yvec,
        zvec = domain_info$indicators[[j]]
      )
    })

    return(list(
      type = "multi",
      labels = domain_info$labels,
      estimates = est_list
    ))
  }

  stop("Unsupported domain mode returned by `standardize_zcol()`.",
       call. = FALSE)
}
