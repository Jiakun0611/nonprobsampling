#' Confidence Intervals for Pseudo-Weighted Estimates
#'
#' Computes normal-approximation confidence intervals for pseudo-weighted
#' means or category proportions returned by \code{\link{pwmean}}.
#'
#' @param object An object of class \code{"pwmean"} or
#'   \code{"pwmean_factor"} returned by \code{\link{pwmean}}.
#' @param parm A character vector of estimate names, as returned by
#'   \code{names(stats::coef(object))}, or a numeric vector of positive
#'   integer positions. If omitted, all estimates are included.
#' @param level A single numeric confidence level strictly between 0 and 1.
#'   Defaults to \code{0.95}.
#' @param ... Additional arguments, currently unused.
#'
#' @details
#' Intervals use the pseudo-weighted estimates and standard errors stored in
#' \code{object$estimates}, with standard normal critical values. At the
#' default confidence level, the intervals match the stored
#' \code{adjusted_lower} and \code{adjusted_upper} columns.
#'
#' Row names match \code{names(stats::coef(object))}. Objects of class
#' \code{"pwmean_factor"} inherit this method. Intervals for category
#' proportions are not truncated to the range from 0 to 1, consistent with
#' the intervals stored by \code{pwmean()}.
#'
#' @return A numeric matrix with one row per requested estimate and two
#'   columns containing the lower and upper confidence limits. Column names
#'   give the corresponding tail probabilities as percentages. Selecting
#'   one estimate still returns a matrix.
#'
#' @seealso \code{\link[stats]{confint}}, \code{\link{coef.pwmean}},
#'   \code{\link{pwmean}}
#' @importFrom stats confint
#' @export
confint.pwmean <- function(object, parm, level = 0.95, ...) {
  if (!is.numeric(level) || length(level) != 1L ||
      !is.finite(level) || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  estimates <- stats::coef(object)
  estimate_names <- names(estimates)
  if (missing(parm)) {
    positions <- seq_along(estimates)
  } else if (is.character(parm)) {
    positions <- match(parm, estimate_names)
    if (anyNA(positions)) {
      stop("`parm` contains unknown estimate names.", call. = FALSE)
    }
  } else if (is.numeric(parm)) {
    if (any(!is.finite(parm) | parm != floor(parm) |
            parm < 1 | parm > length(estimates))) {
      stop("`parm` must contain valid positive integer positions.",
           call. = FALSE)
    }
    positions <- parm
  } else {
    stop("`parm` must be a character or numeric vector.", call. = FALSE)
  }

  se <- object[["estimates"]][["adjusted_se"]]
  probabilities <- c((1 - level) / 2, (1 + level) / 2)
  margin <- stats::qnorm(probabilities[[2L]]) * se
  intervals <- cbind(estimates - margin, estimates + margin)
  dimnames(intervals) <- list(
    estimate_names,
    paste0(format(100 * probabilities, trim = TRUE, digits = 3), "%")
  )
  intervals[positions, , drop = FALSE]
}
