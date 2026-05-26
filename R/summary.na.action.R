#' Extract NA action from a pw_fit object
#'
#' Returns the \code{na.action} component recorded during the build step.
#'
#' @param object An object of class \code{"pw_fit"} returned by \code{\link{est_pw}}.
#' @param ... Additional arguments (not used).
#'
#' @method na.action pw_fit
#' @importFrom stats na.action
#' @export
na.action.pw_fit <- function(object, ...) {
  object$internal$na$na_action_obj
}


#' Extract NA action from a pwmean object
#'
#' Returns the \code{na.action} component recorded during estimation,
#' mimicking \code{\link{stats}{na.action}} behavior for fitted model objects.
#'
#' @param object An object of class \code{"pwmean"} returned by \code{\link{pwmean}}.
#' @param ... Additional arguments (not used).
#'
#' @method na.action pwmean
#' @importFrom stats na.action
#' @export
na.action.pwmean <- function(object, ...) {
  object$na.action
}
