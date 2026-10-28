#' Extract Pseudo-Weighted Means or Category Proportions
#'
#' Extracts pseudo-weighted estimates from an object returned by
#' \code{\link{pwmean}}.
#'
#' @param object An object of class \code{"pwmean"} or
#'   \code{"pwmean_factor"} returned by \code{\link{pwmean}}.
#' @param ... Additional arguments, currently unused.
#'
#' @details
#' Values are taken from \code{object$estimates$adjusted_mean}, in row order.
#' For numeric outcomes, names are the domain labels, including
#' \code{"Overall"} when no domain variable was supplied.
#'
#' For categorical outcomes, values are category prevalences or proportions.
#' Names combine the category and domain labels separated by \code{" | "},
#' including \code{"Overall"} when no domain variable was supplied.
#' Objects of class \code{"pwmean_factor"} inherit this method.
#'
#' Unweighted estimates remain available in \code{object$estimates}.
#' Participation model coefficients can be extracted from the original
#' \code{"pw_fit"} object using \code{\link{coef.pw_fit}}.
#'
#' @return A named numeric vector of pseudo-weighted means for numeric
#'   outcomes, or category prevalences or proportions for categorical outcomes.
#'
#' @seealso \code{\link[stats]{coef}}, \code{\link{pwmean}},
#'   \code{\link{coef.pw_fit}}
#' @importFrom stats coef
#' @export
coef.pwmean <- function(object, ...) {
  estimates <- object[["estimates"]]
  labels <- estimates[["domain"]]
  if ("category" %in% names(estimates)) {
    labels <- paste(estimates[["category"]], labels, sep = " | ")
  }
  setNames(estimates[["adjusted_mean"]], labels)
}
