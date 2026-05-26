#' Extract and align sc data from a build object
#'
#' Reads the raw sc sample, the build-stage complete-case index
#' (`keep_sc`), the covariate matrix `Xc`, and the pseudo-weights from
#' `build`. Remove NA placeholders inserted by `na.exclude` so that all
#' returned vectors and matrices have the same length (the number of
#' build-stage complete cases).
#'
#' @param build A `pw_fit` object returned by the build step.
#'
#' @return A list with components:
#'
#' - `sc`: data frame of build-stage complete cases.
#' - `X`: covariate matrix `build$internal$Xc`.
#' - `w`: pseudo-weight vector with NA placeholders removed.
#' - `idx_keep`: integer index of which rows of `raw_sc` were retained.
#'
#' @keywords internal
prepare_sc_data <- function(build) {

  raw_sc  <- build$internal$raw_sc
  keep_sc <- build$internal$na$keep_sc

  if (!is.logical(keep_sc))
    stop("prepare_sc_data: keep_sc must be a logical vector.", call. = FALSE)
  idx_keep <- which(keep_sc)
  sc_keep  <- raw_sc[idx_keep, , drop = FALSE]

  X <- build$internal$Xc
  w <- build$pseudo_weights

  # for na.exclude builds, pseudo_weights has NA placeholders for dropped rows;
  # remove them here so w aligns with the complete-case rows in X and sc_keep
  w <- w[!is.na(w)]

  nx <- nrow(X)
  if (is.null(nx)) stop("prepare_sc_data: nrow(build$Xc) is NULL (Xc is not a matrix/data.frame).", call. = FALSE)

  if (nx != length(w)) {
    stop("prepare_sc_data: mismatch between nrow(build$Xc) and length(build$pseudo_weights).", call. = FALSE)
  }
  if (nrow(sc_keep) != length(w)) {
    stop("prepare_sc_data: mismatch between nrow(sc_keep) and length(build$pseudo_weights).", call. = FALSE)
  }

  list(sc = sc_keep, X = X, w = w, idx_keep = idx_keep)
}
