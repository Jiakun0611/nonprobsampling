#' Estimate Pseudo-Weighted Means, Prevalences, and Standard Errors
#'
#' @description
#' Computes pseudo-weighted means and standard errors using a fitted
#' pseudo-weight object of class \code{"pw_fit"} returned by
#' \code{\link{est_pw}}. The function applies second-layer missing-data
#' handling for the outcome and optional domain variable, and then estimates
#' overall or domain-specific means or prevalences using the pseudo-weighting
#' method stored in \code{object}.
#'
#' @details
#' \strong{Missing data handling (layer 2).}
#' After pseudo-weights are constructed by \code{est_pw()}, estimation of the
#' mean requires complete cases for the outcome \code{y} and, if supplied, the
#' domain variable \code{zcol}. The argument \code{na.action} controls how
#' these missing values are handled at the outcome-estimation step.
#'
#' \strong{Input \code{object}.}
#' The \code{object} argument should be an object of class \code{"pw_fit"}
#' returned by \code{\link{est_pw}}. It stores the estimated pseudo-weights,
#' participation model information, and design-based quantities required for point
#' and variance estimation.
#'
#' \strong{Categorical outcomes.}
#' When \code{y} is a categorical variable (defined as a factor in R),
#' \code{pwmean()} estimates the prevalence (proportion) of each category.
#' To do so, each category is internally converted into a 0/1 indicator
#' variable, and the pseudo-weighted mean estimator is then computed for each
#' indicator.
#'
#' @usage
#' pwmean(object, y, zcol = NULL, na.action = stats::na.omit)
#'
#' @param object An object of class \code{"pw_fit"} returned by
#'   \code{\link{est_pw}}.
#' @param y A character string specifying the name of the outcome variable in the
#'   nonprobability sample stored in \code{object}. The outcome must be numeric
#'   for mean estimation, including binary 0/1 outcomes for prevalence
#'   estimation, or a factor for category prevalence estimation.
#' @param zcol Optional character string giving the name of a categorical domain
#'   variable in the nonprobability sample stored in the \code{object}. If
#'   \code{NULL}, the overall mean is estimated. If supplied, estimates are
#'   computed within domains defined by this variable. The following column
#'   types are supported: \code{logical} (must contain both \code{TRUE} and
#'   \code{FALSE}); \code{numeric} or \code{integer} containing only \code{0}
#'   and \code{1} after removing missing values; \code{character} (empty
#'   strings are treated as missing values); and \code{factor} (unused levels
#'   are dropped).
#' @param na.action Function specifying how missing values in \code{y} and
#'   \code{zcol} should be handled. Default is \code{stats::na.omit}.
#'
#' @return
#' An object of class \code{"pwmean"} containing unweighted and pseudo-weighted
#' estimates, standard errors, and confidence intervals. For categorical outcomes,
#' the estimate columns contain category prevalences.
#'
#' \describe{
#'   \item{\code{method}}{
#'     Character. The pseudo-weighting method used.
#'   }
#'
#'   \item{\code{estimates}}{
#'     A data frame containing the unweighted and pseudo-weighted estimates.
#'
#'     For numeric outcomes, the first column is \code{domain}. If
#'     \code{zcol = NULL}, \code{domain} is \code{"Overall"}. If \code{zcol}
#'     is a \code{logical} variable or a \code{numeric}/\code{integer}
#'     variable containing only \code{0} and \code{1}, there is one row with
#'     \code{domain} labeled \code{"<zcol> = 1"}. If \code{zcol} is a
#'     \code{factor} or \code{character} variable, there is one row per
#'     \code{zcol} level, with \code{domain} labeled
#'     \code{"<zcol> = <level>"}.
#'
#'     For categorical outcomes, the first two columns are
#'     \code{category} and \code{domain}. \code{category} identifies the
#'     outcome level as \code{"<y> = <level>"}. If \code{zcol = NULL},
#'     \code{domain} is \code{"Overall"} for each outcome level. If
#'     \code{zcol} is supplied, the rows are formed by each outcome category
#'     within each domain, and \code{domain} follows the same labels described
#'     above for \code{zcol}.
#'
#'     The columns are:
#'     \describe{
#'       \item{\code{category}}{Category label for categorical outcomes only.}
#'       \item{\code{domain}}{Domain label.}
#'       \item{\code{unweighted_mean}, \code{unweighted_se}}{Unweighted
#'         mean of \code{y} and its standard error.}
#'       \item{\code{unweighted_lower}, \code{unweighted_upper}}{Bounds of the
#'         95\% confidence interval for the unweighted mean, based on the
#'         normal approximation.}
#'       \item{\code{adjusted_mean}, \code{adjusted_se}}{Pseudo-weighted mean
#'         of \code{y} and its standard error.}
#'       \item{\code{adjusted_lower}, \code{adjusted_upper}}{Bounds of the
#'         95\% confidence interval for the pseudo-weighted mean, based on the
#'         normal approximation.}
#'     }
#'   }
#'
#'   \item{\code{na.action}}{
#'     Integer vector of row indices omitted at the outcome-estimation step,
#'     with class \code{"omit"} or \code{"exclude"} matching the
#'     \code{na.action} argument, or \code{NULL} if no observations were
#'     omitted. The indices refer to the nonprobability sample available to
#'     \code{pwmean()} after missing-data handling in
#'     \code{est_pw()}.
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
#' \donttest{
#' data(sc)
#' data(sp1)
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
#'   method    = "calibration",
#'   control   = pw_solver_control(ftol=1e-6)
#' )
#'
#' out <- pwmean(fit, y = "psa_level", zcol = "BMI")
#'
#' print(out)
#'
#' summary(out)
#' }
#'
#' @export
#' @name pwmean
NULL
