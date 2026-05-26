#' Construct block-diagonal D matrix for multi-reference raking
#'
#' Internal helper to assemble the block-diagonal covariance matrix
#' \eqn{D} for the multi-reference raking estimator. Each reference survey
#' contributes one design-based covariance block computed from
#' `compute_D_raking()`.
#'
#' @param sp_des_list A non-empty list of survey design objects, each of class
#'   `"survey.design2"` or `"svyrep.design"`.
#' @param Xp_list A list of reference-sample design matrices, one for each
#'   reference survey. Each matrix must have the same number of rows as the
#'   corresponding survey design object in `sp_des_list`.
#'
#' @return A block-diagonal covariance matrix \eqn{D}.
#' @keywords internal
make_block_D_multi <- function(sp_des_list, Xp_list) {

  n <- length(sp_des_list)

  if (!is.list(sp_des_list) || n == 0) {
    stop("'sp_des_list' must be a non-empty list of survey design objects.",
         call. = FALSE)
  }

  if (!is.list(Xp_list) || length(Xp_list) != n) {
    stop("'Xp_list' must be a list with the same length as 'sp_des_list'.",
         call. = FALSE)
  }

  ok_des <- vapply(
    sp_des_list,
    function(x) inherits(x, c("survey.design2", "svyrep.design")),
    logical(1)
  )
  if (!all(ok_des)) {
    bad <- which(!ok_des)
    stop(
      sprintf(
        "Elements %s of 'sp_des_list' are not valid survey design objects.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  block_sizes <- integer(n)

  for (i in seq_len(n)) {
    Xp_i <- Xp_list[[i]]

    if (!is.matrix(Xp_i)) {
      Xp_i <- as.matrix(Xp_i)
    }

    n_sp <- nrow(sp_des_list[[i]]$variables)
    if (nrow(Xp_i) != n_sp) {
      stop(
        sprintf(
          "Reference survey %d: nrow(Xp_list[[%d]]) = %d but design has %d observations.",
          i, i, nrow(Xp_i), n_sp
        ),
        call. = FALSE
      )
    }

    Xp_list[[i]] <- Xp_i
    block_sizes[i] <- ncol(Xp_i)
  }

  total_dim <- sum(block_sizes)
  starts <- cumsum(c(1, utils::head(block_sizes, -1)))
  ends   <- cumsum(block_sizes)

  D <- matrix(0, total_dim, total_dim)

  for (i in seq_len(n)) {
    D_i <- compute_D_raking(
      sp_des     = sp_des_list[[i]],
      Xp         = Xp_list[[i]]
    )

    D_i <- as.matrix(D_i)
    idx <- starts[i]:ends[i]

    if (!all(dim(D_i) == c(length(idx), length(idx)))) {
      stop(
        sprintf(
          "Reference survey %d: dimension mismatch. D_i is %d x %d, but block expects %d x %d.",
          i, nrow(D_i), ncol(D_i), length(idx), length(idx)
        ),
        call. = FALSE
      )
    }

    D[idx, idx] <- D_i
  }

  D
}
