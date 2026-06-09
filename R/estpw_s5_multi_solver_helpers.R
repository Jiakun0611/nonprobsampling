#' Map each reference-sample block to its column indices in Xc
#'
#' @param Xc Numeric design matrix for the nonprobability sample, with named
#'   columns.
#' @param Xp_list List of reference-sample design matrices, each with named
#'   columns that are a subset of `colnames(Xc)`.
#' @param label Character scalar used as a prefix in error messages.
#'
#' @return A list of integer vectors, one per element of `Xp_list`, giving the
#'   column positions in `Xc` that correspond to the columns of each
#'   `Xp_list[[j]]`.
#'
#' @keywords internal
multi_raking_block_cols <- function(Xc, Xp_list, label = "Multi_Calibration") {
  if (is.null(colnames(Xc))) {
    stop(
      sprintf("[%s] Xc must have column names for multi-reference calibration.", label),
      call. = FALSE
    )
  }

  lapply(seq_along(Xp_list), function(j) {
    Xp <- Xp_list[[j]]

    if (is.null(colnames(Xp))) {
      stop(
        sprintf("[%s] Xp_list[[%d]] must have column names.", label, j),
        call. = FALSE
      )
    }

    idx <- match(colnames(Xp), colnames(Xc))

    if (any(is.na(idx))) {
      missing_cols <- colnames(Xp)[is.na(idx)]
      stop(
        sprintf(
          paste0(
            "[%s] Some columns in Xp_list[[%d]] are not found in Xc: %s.\n",
            "This usually means the multi-reference design matrices are not aligned."
          ),
          label, j, paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    idx
  })
}

#' Compute reference weighted totals for multi-reference calibration
#'
#' @param Xp_list List of reference-sample design matrices.
#' @param wts_list List of numeric weight vectors, one per element of
#'   `Xp_list`.
#' @param label Character scalar used as a prefix in error messages.
#'
#' @return A named numeric vector of length equal to the total number of
#'   columns across all elements of `Xp_list`, containing the survey-weighted
#'   column totals \eqn{\sum_j w_{ij} X_{ij}} stacked in block order.
#'
#' @keywords internal
multi_raking_fp <- function(Xp_list, wts_list, label = "Multi_Calibration") {
  if (length(Xp_list) != length(wts_list)) {
    stop(
      sprintf(
        "[%s] Xp_list and wts_list must have the same length.",
        label
      ),
      call. = FALSE
    )
  }

  unlist(
    lapply(seq_along(Xp_list), function(j) {
      Xp <- Xp_list[[j]]
      wts <- wts_list[[j]]

      if (nrow(Xp) != length(wts)) {
        stop(
          sprintf(
            "[%s] nrow(Xp_list[[%d]]) does not match length(wts_list[[%d]]).",
            label, j, j
          ),
          call. = FALSE
        )
      }

      out <- colSums(wts * Xp)

      if (any(!is.finite(out))) {
        stop(
          sprintf(
            "[%s] Reference weighted totals contain non-finite values for Xp_list[[%d]].",
            label, j
          ),
          call. = FALSE
        )
      }

      out
    }),
    use.names = TRUE
  )
}

#' Compute starting values for multi-reference calibration
#'
#' @param Xc Numeric design matrix for the nonprobability sample.
#' @param Xp_list List of reference-sample design matrices.
#' @param block_cols List of integer vectors mapping each reference-sample
#'   block to its column indices in `Xc`, as returned by
#'   `multi_raking_block_cols`.
#' @param f_p Named numeric vector of reference weighted totals, as returned
#'   by `multi_raking_fp`.
#' @param label Character scalar used as a prefix in error messages.
#'
#' @return A numeric vector of length `ncol(Xc)` containing the starting
#'   values for the coefficient vector \eqn{\beta}, with the intercept
#'   initialized to match the ratio of reference to sample weighted totals
#'   and all other coefficients set to zero.
#'
#' @keywords internal
multi_raking_start <- function(Xc, Xp_list,
                               block_cols,
                               f_p,
                               label = "Multi_Calibration") {
  p_dim <- ncol(Xc)
  beta_start <- rep(0, p_dim)

  eta <- as.vector(Xc %*% beta_start)

  if (any(!is.finite(eta))) {
    stop(
      sprintf("[%s] Cannot construct initial values because initial linear predictor is non-finite.", label),
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      sprintf("[%s] Cannot construct initial values because initial pseudo-weights are non-finite.", label),
      call. = FALSE
    )
  }

  f_c <- unlist(
    lapply(block_cols, function(cols) {
      colSums(wts_sc * Xc[, cols, drop = FALSE])
    }),
    use.names = TRUE
  )

  if (length(f_p) != length(f_c)) {
    stop(
      sprintf(
        paste0(
          "[%s] Cannot construct initial values because the number of reference totals ",
          "(%d) does not match the number of sample-side totals (%d)."
        ),
        label, length(f_p), length(f_c)
      ),
      call. = FALSE
    )
  }

  if (!is.finite(f_p[1]) || !is.finite(f_c[1]) || f_p[1] <= 0 || f_c[1] <= 0) {
    stop(
      sprintf(
        paste0(
          "[%s] Cannot construct initial intercept because intercept totals are invalid. ",
          "Check survey weights and intercept totals."
        ),
        label
      ),
      call. = FALSE
    )
  }

  beta_start[1] <- -log(f_p[1] / f_c[1])

  if (any(!is.finite(beta_start))) {
    stop(
      sprintf("[%s] Initial coefficient vector contains non-finite values.", label),
      call. = FALSE
    )
  }

  beta_start
}


multi_raking_fn <- function(beta, Xc,
                            f_p,
                            block_cols,
                            label = "Multi_Calibration",
                            ...) {
  eta <- as.vector(Xc %*% beta)

  if (any(!is.finite(eta))) {
    stop(
      sprintf("[%s] Multi-reference calibration failed: linear predictor became non-finite.", label),
      call. = FALSE
    )
  }

  if (any(eta < -log(.Machine$double.xmax))) {
    stop(
      sprintf(
        paste(
          "[%s] Multi-reference calibration failed: exp(-eta) overflow.",
          "This usually indicates unstable coefficient values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      sprintf("[%s] Multi-reference calibration failed: pseudo-weights became non-finite.", label),
      call. = FALSE
    )
  }

  f_c <- unlist(
    lapply(block_cols, function(cols) {
      colSums(wts_sc * Xc[, cols, drop = FALSE])
    }),
    use.names = TRUE
  )

  if (length(f_c) != length(f_p)) {
    stop(
      sprintf(
        "[%s] Estimating-equation length mismatch: length(f_c) = %d, length(f_p) = %d.",
        label, length(f_c), length(f_p)
      ),
      call. = FALSE
    )
  }

  f_c - f_p
}

multi_raking_jac <- function(beta, Xc,
                             f_p,
                             block_cols,
                             label = "Multi_Calibration",
                             tol_singular = 1e-7,
                             ...) {
  eta <- as.vector(Xc %*% beta)

  if (any(!is.finite(eta))) {
    stop(
      sprintf("[%s] Multi-reference calibration failed: linear predictor became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  if (any(eta < -log(.Machine$double.xmax))) {
    stop(
      sprintf(
        paste(
          "[%s] Multi-reference calibration failed: exp(-eta) overflow when computing Jacobian.",
          "This usually indicates unstable coefficient values."
        ),
        label
      ),
      call. = FALSE
    )
  }

  wts_sc <- as.vector(exp(-eta))

  if (any(!is.finite(wts_sc))) {
    stop(
      sprintf("[%s] Multi-reference calibration failed: pseudo-weights became non-finite when computing Jacobian.", label),
      call. = FALSE
    )
  }

  f_c <- unlist(
    lapply(block_cols, function(cols) {
      colSums(wts_sc * Xc[, cols, drop = FALSE])
    }),
    use.names = TRUE
  )

  g <- f_c - f_p

  J <- do.call(
    rbind,
    lapply(block_cols, function(cols) {
      Xc_block <- Xc[, cols, drop = FALSE]
      -t(wts_sc * Xc_block) %*% Xc
    })
  )

  check_estimating_system(
    J = J,
    g = g,
    label = label,
    tol_singular = tol_singular
  )

  J
}


