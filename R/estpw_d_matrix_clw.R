#' Compute design-based covariance matrix D for CLW
#'
#' Internal helper to compute
#' \deqn{D = Var_p\left( \sum_{j \in s_p} d_j \pi_j x_j \right)}
#' from an existing survey design object created by
#' survey::svydesign() or survey::svrepdesign().
#'
#' In the CLW method, the probability sample contribution in the
#' estimating equation is \eqn{\pi_j d_j x_j}, where `pi_j` corresponds
#' to the inverse of the estimated pseudo-weight. Thus the design-based
#' covariance matrix is obtained by treating \eqn{h_j = \pi_j x_j}
#' as the survey total variable.
#'
#' @param sp_des A survey design object of class `"survey.design2"` or
#'   `"svyrep.design"`.
#' @param pi_sp Numeric vector of estimated values multiplying `X_p`
#'   in the CLW estimating equation. Length must equal the number of
#'   rows in `sp_des$variables`.
#' @param Xp Numeric matrix of dimension `n_p x p`. Each row is the
#'   covariate vector `x_j` used in the CLW estimating equation.
#'
#' @return A `p x p` covariance matrix `D`.
#' @keywords internal
compute_D_CLW <- function(sp_des, pi_sp, Xp) {

  if (!inherits(sp_des, c("survey.design2", "svyrep.design"))) {
    stop(
      "'sp_des' must be a survey design object from svydesign() or svrepdesign().",
      call. = FALSE
    )
  }

  if (!is.matrix(Xp)) {
    Xp <- as.matrix(Xp)
  }

  n_sp <- nrow(sp_des$variables)

  if (length(pi_sp) != n_sp) {
    stop(
      sprintf(
        "Length mismatch: length(pi_sp) = %d but the survey design object contains %d observations.",
        length(pi_sp), n_sp
      ),
      call. = FALSE
    )
  }

  if (nrow(Xp) != n_sp) {
    stop(
      sprintf(
        "Row mismatch: nrow(Xp) = %d but the survey design object contains %d observations.",
        nrow(Xp), n_sp
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(pi_sp)) {
    stop("'pi_sp' must be numeric.", call. = FALSE)
  }

  if (anyNA(pi_sp)) {
    stop("'pi_sp' contains NA values.", call. = FALSE)
  }

  if (anyNA(Xp)) {
    stop("'Xp' contains NA values.", call. = FALSE)
  }

  # safe column names for formula parsing
  xp_names <- colnames(Xp)
  if (is.null(xp_names)) {
    xp_names <- paste0("x", seq_len(ncol(Xp)))
  }
  xp_names <- make.names(xp_names, unique = TRUE)

  # CLW h_j = pi_j * x_j
  H <- sweep(Xp, 1, pi_sp, "*")

  H_df <- as.data.frame(H)
  colnames(H_df) <- paste0("h_", xp_names)

  # attach h_j columns to existing survey design object
  sp_des_h <- sp_des
  sp_des_h$variables <- cbind(sp_des_h$variables, H_df)

  # no intercept: H already contains all columns of h_j
  fml <- stats::reformulate(colnames(H_df), intercept = FALSE)

  tot <- survey::svytotal(fml, design = sp_des_h)
  D   <- stats::vcov(tot)

  return(D)
}
