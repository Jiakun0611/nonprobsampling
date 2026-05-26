#' Estimate Pseudo-Weighted Mean and Variance
#'
#' @description
#' Computes the pseudo-weighted mean and its variance using a fitted
#' pseudo-weight object of class \code{"pw_fit"} returned by
#' \code{\link{est_pw}}. This function applies
#' second-layer missing-data handling for the outcome and optional domain
#' variables, then performs estimation under the selected method
#' (ALP/CLW/calibration/multi) fitted by \code{est_pw()}.
#'
#' @details
#' \strong{Missing data handling (layer 2).}
#' After pseudo-weights are constructed via \code{est_pw()}, estimation of the
#' mean requires complete cases for the outcome \code{y} and optional domain
#' variable \code{zcol}.
#'
#' \strong{Input \code{object}.}
#' The \code{object} argument should be an object of class \code{"pw_fit"}
#' returned by \code{\link{est_pw}}. It stores participation model objects and
#' cached matrices required for estimation and variance calculation. This design
#' separates pseudo-weight construction from outcome estimation.
#'
#' @param object An object of class \code{"pw_fit"} returned by
#'   \code{\link{est_pw}}.
#' @param y A character string giving the name of the outcome variable in the
#'   nonprobability sample stored in \code{object}.
#' @param zcol Optional character string giving the name of a domain variable
#'   in the nonprobability sample stored in \code{object}. It is used for
#'   subgroup estimation.
#' @param na.action Function specifying how missing values in \code{y} and
#'   \code{zcol} should be handled. Default is \code{stats::na.omit}.
#'
#' @return
#' An object of class \code{"pwmean"} containing unweighted and pseudo-weighted
#' estimates, standard errors, and confidence intervals.
#'
#' \describe{
#'   \item{\code{method}}{
#'     Character. The pseudo-weighting method used.
#'   }
#'
#'   \item{\code{unweighted}}{
#'     A list of unweighted estimates based on the original nonprobability
#'     sample:
#'     \describe{
#'       \item{\code{mean}}{Unweighted mean of \code{y}.}
#'       \item{\code{se}}{Standard error of the unweighted mean.}
#'       \item{\code{CI_95}}{95\% confidence interval.}
#'     }
#'   }
#'
#'   \item{\code{adjusted}}{
#'     A list of pseudo-weighted estimates:
#'     \describe{
#'       \item{\code{mean}}{Pseudo-weighted mean.}
#'       \item{\code{se}}{Standard error.}
#'       \item{\code{CI_95}}{95\% confidence interval.}
#'     }
#'   }
#'
#'   \item{\code{domains}}{
#'     If \code{zcol} is a multi-level domain variable, a data frame containing
#'     domain-level unweighted and pseudo-weighted estimates.
#'   }
#'
#'   \item{\code{na.action}}{
#'     Integer vector of omitted row indices (from the NA-filtered
#'     nonprobability sample), with class \code{"omit"} or \code{"exclude"}
#'     matching the \code{na.action} argument, or \code{NULL} if no
#'     observations were omitted. This element has the same structure as the
#'     \code{na.action} element of a fitted \code{\link[stats]{lm}} object.
#'   }
#'
#'   \item{\code{call}}{
#'     The matched function call.
#'   }
#' }
#'
#' @seealso
#' \code{\link{est_pw}},
#' \code{\link{summary.pwmean}},
#' \code{\link{print.pwmean}}
#'
#' @examples
#' data(sc)
#' data(sp1)
#'
#'
#' ref1_design <- survey::svydesign(
#'   ids     = ~psu_sp1,
#'   strata  = ~strata_sp1,
#'   weights = ~wts_sp1,
#'   data    = sp1,
#'   nest    = TRUE
#' )
#'
#' fit <- est_pw(
#'   data      = list(sc, ref1_design),
#'   p_formula = ~ agecat + race + education + comorbidity + BMI + diabetes,
#'   method    = "calibration"
#' )
#'
#' pwmean(fit, y = "psa_level", zcol = "BMI")
#' out <- pwmean(fit, y = "psa_level", zcol = "BMI")
#' summary(out)
#'
#' @export
#' @name pwmean
NULL
