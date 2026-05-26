#' Run the estimator for a single domain
#'
#' Selects and calls the specific estimator function based on
#' `build$method`, passing the relevant matrices and vectors
#' for one domain at a time.
#'
#' @param build A `pw_fit` object returned by the build step.
#' @param Y Outcome vector of length `n`.
#' @param zvec Domain indicator vector of length `n`.
#' @param w Pseudo-weight vector of length `n`.
#' @param X Design matrix for the convenience sample, with dimension
#'  `n x p`.
#'
#' @return A list with components `mean` and `variance`.
#'
#' @keywords internal
dispatch_estimator_one_domain <- function(build, Y, zvec, w, X) {

  if (length(zvec) != length(Y)) {
    stop("Length mismatch between `Y` and `zvec`.", call. = FALSE)
  }

  method <- build$method
  D <- build$internal$D
  S_beta <- build$internal$S_beta

  method_key <- tolower(method)

  out <- switch(
    method_key,
    "calibration" = raking_estimate(Y, zvec, w, X, D, S_beta),
    "cali"        = raking_estimate(Y, zvec, w, X, D, S_beta),

    "alp"         = alp_estimate(Y, zvec, w, X, D, S_beta),
    "clw"         = clw_estimate(Y, zvec, w, X, D, S_beta),

    "multi"       = multi_estimate(Y, zvec, w, X, D, S_beta),

    stop(
      sprintf(
        "Unknown method '%s'. Must be one of: 'alp', 'clw', 'calibration'/'cali', 'multi'.",
        method
      ),
      call. = FALSE
    )
  )

  out
}

#' Dispatch the estimator across domains
#'
#' Determines the domain mode (overall, binary, or factor) from
#' `yz_data` and calls `dispatch_estimator_one_domain()` for each domain,
#' returning a unified result list.
#'
#' @param build A `pw_fit` object returned by the build step.
#' @param yz_data A list returned by `process_na_yz()`, containing
#'   `Y`, `w`, `X`, and `domain`.
#'
#' @return A list with components:
#'
#' - `type`: either `"single"` or `"multi"`.
#' - `labels`: character vector of domain labels.
#' - `estimates`: a list of per-domain results, each with `mean` and `variance`.
#'
#' @keywords internal
dispatch_estimator <- function(build, yz_data) {

  Y <- yz_data$Y
  w <- yz_data$w
  X <- yz_data$X

  domain <- yz_data$domain
  mode <- domain$mode

  #----------------------------------#
  # overall mean
  #----------------------------------#
  if (identical(mode, "overall")) {
    zvec <- rep.int(1L, length(Y))

    est <- dispatch_estimator_one_domain(
      build = build,
      Y = Y,
      zvec = zvec,
      w = w,
      X = X
    )

    return(list(
      type = "single",
      labels = "Overall",
      estimates = est
    ))
  }

  #----------------------------------#
  # binary domain variable
  #----------------------------------#
  if (identical(mode, "binary")) {
    zvec <- domain$indicators[[1]]

    est <- dispatch_estimator_one_domain(
      build = build,
      Y = Y,
      zvec = zvec,
      w = w,
      X = X
    )

    return(list(
      type = "single",
      labels = domain$labels,
      estimates = est
    ))
  }

  #----------------------------------#
  # factor / character domain variable
  #----------------------------------#
  if (identical(mode, "factor")) {
    ind_df <- domain$indicators
    labs <- domain$labels

    est_list <- lapply(seq_along(labs), function(j) {
      dispatch_estimator_one_domain(
        build = build,
        Y = Y,
        zvec = ind_df[[j]],
        w = w,
        X = X
      )
    })

    return(list(
      type = "multi",
      labels = labs,
      estimates = est_list
    ))
  }

  stop("Unsupported domain mode in `yz_data$domain`.", call. = FALSE)
}
