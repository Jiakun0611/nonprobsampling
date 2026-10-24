#' Extract Participation Model Coefficients
#'
#' Extracts the participation model coefficients estimated by
#' \code{\link{est_pw}}.
#'
#' @param object An object of class \code{"pw_fit"} returned by
#'   \code{\link{est_pw}}.
#' @param ... Additional arguments, currently unused.
#'
#' @return A named numeric vector of participation model coefficients,
#'   in the order stored in \code{object$coefficients}. Returns \code{NULL}
#'   if the object does not contain coefficients.
#'
#' @seealso \code{\link[stats]{coef}}, \code{\link{est_pw}}
#' @importFrom stats coef
#' @export
coef.pw_fit <- function(object, ...) {
  object[["coefficients"]]
}
