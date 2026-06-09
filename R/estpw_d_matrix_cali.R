#' Compute design-based covariance matrix D for calibration
#'
#' Internal helper to compute
#' \deqn{D = Var_p\left( \sum_{j \in s_p} d_j x_j \right)}
#' from an existing survey design object created by
#' survey::svydesign() or survey::svrepdesign().
#'
#' In the raking ratio calibration method, the probability sample
#' contribution in the estimating equation is \eqn{d_j x_j}, so the
#' design-based covariance matrix is obtained by treating
#' \eqn{h_j = x_j} as the survey total variable.
#'
#' @param sp_des A survey design object of class `"survey.design2"` or
#'   `"svyrep.design"`.
#' @param Xp Numeric matrix of dimension `n_p x p`. Each row is the
#'   covariate vector `x_j` used in the calibration estimating equation.
#'
#' @return A `p x p` covariance matrix `D`.
#' @keywords internal
compute_D_raking <- function(sp_des, Xp) {

  if (!inherits(sp_des, c("survey.design2", "svyrep.design"))) {
    stop("'sp_des' must be a survey design object from svydesign() or svrepdesign().",
         call. = FALSE)
  }

  if (!is.matrix(Xp)) {
    Xp <- as.matrix(Xp)
  }

  n_sp <- nrow(sp_des$variables)
  if (nrow(Xp) != n_sp) {
    stop(sprintf(
      "Row mismatch: nrow(Xp) = %d but the survey design object contains %d observations.",
      nrow(Xp), n_sp
    ), call. = FALSE)
  }

  # safe column names for formula parsing
  xp_names <- colnames(Xp)
  if (is.null(xp_names)) {
    xp_names <- paste0("x", seq_len(ncol(Xp)))
  }
  xp_names <- make.names(xp_names, unique = TRUE)

  H_df <- as.data.frame(Xp)
  colnames(H_df) <- paste0("h_", xp_names)

  # attach estimating equation columns to the existing design object
  sp_des_h <- sp_des
  sp_des_h$variables <- cbind(sp_des_h$variables, H_df)

  # no intercept
  fml <- stats::reformulate(colnames(H_df), intercept = FALSE)

  tot <- survey::svytotal(fml, sp_des_h)
  D <- stats::vcov(tot)

  return(D)
}
