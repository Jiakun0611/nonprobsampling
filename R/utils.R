# utils.R
# General-purpose utility functions shared across build and estimation stages.


# Logistic (inverse-logit) transformation.
# Used by ALP, CLW, and raking solver helpers.
#
# Implemented via stats::plogis() rather than the algebraically equivalent
# exp(x) / (1 + exp(x)). The naive form overflows to Inf once x exceeds about
# 710, so exp(x) / (1 + exp(x)) evaluates to NaN in that range; that NaN then
# trips the "non-finite fitted probabilities" guards in the ALP/CLW solvers and
# turns a merely large linear predictor into a hard error. stats::plogis() is
# numerically stable across the whole real line and saturates to 0 or 1 in the
# tails instead of returning NaN.
expit <- function(x) {
  stats::plogis(x)
}


# Null-coalescing operator: return `a` if non-NULL, otherwise `b`.
# Used in S3 print/summary methods.
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Check that a design matrix X is full column rank; stop with a clear message
# if rank-deficient. Used by all one-reference and multi-reference builders.
check_design_identifiability <- function(X, label = "method", tol = 1e-7) {
  qx <- qr(X, tol = tol)
  if (qx$rank < ncol(X)) {
    stop(
      sprintf(
        paste0("[%s] Model matrix is rank-deficient before NR iteration ",
               "(rank = %d < %d). This suggests collinearity or redundant terms ",
               "in p_formula. Please simplify the model."),
        label, qx$rank, ncol(X)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# Construct a design matrix from `vars` columns of `data`, optionally prepending
# an intercept column. Used by all one-reference and multi-reference builders.
add_intercept <- function(vars, data, intercept = TRUE) {
  if (length(vars) == 0) {
    stop("No variables provided for design matrix.")
  }

  miss <- setdiff(vars, colnames(data))
  if (length(miss) > 0) {
    stop(
      sprintf("Columns not found in data: %s", paste(miss, collapse = ", "))
    )
  }

  X <- as.matrix(data[, vars, drop = FALSE])

  if (intercept) {
    X <- cbind("(Intercept)" = 1, X)
  }

  X
}
