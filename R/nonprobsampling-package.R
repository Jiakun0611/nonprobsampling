#' @keywords internal
#'
#' @description
#' \pkg{nonprobsampling} provides pseudo-weighting methods for finite-population
#' inference from nonprobability samples -- such as convenience samples,
#' volunteer cohorts, and opt-in panels -- using auxiliary information from one
#' or more probability reference surveys. Because the participation mechanism of
#' a nonprobability sample is unknown, unadjusted estimates of population means
#' and prevalences can be badly biased. The package corrects this participation
#' bias by estimating each unit's participation probability from a participation
#' model, converting it into a pseudo-weight, and forming pseudo-weighted
#' estimators of the finite-population mean (or prevalence, for a binary
#' outcome).
#'
#' @details
#' The package implements the general estimating-equation framework of
#' Landsman et al. (2026). Pseudo-weights are obtained by solving a system of
#' estimating equations that matches the population totals of the auxiliary
#' variables estimated from the nonprobability sample (using the unknown
#' participation weights) to their counterparts estimated from the reference
#' survey or surveys (using the known survey sampling weights).
#'
#' Several published one-reference methods arise as special cases of this
#' framework under different choices of the weight and estimating functions:
#' \itemize{
#'   \item the adjusted logistic propensity (ALP) method of
#'     Wang, Valliant, and Li (2021);
#'   \item the Chen-Li-Wu (CLW) method of Chen, Li, and Wu (2020);
#'   \item the raking-ratio calibration method, which equates the
#'     pseudo-weighted totals of the auxiliary variables in the nonprobability
#'     sample to the survey-weighted totals in the reference survey.
#' }
#'
#' The central feature of Landsman et al. (2026) is a multiple-reference
#' extension of the calibration estimator, which integrates auxiliary
#' information across several reference surveys when no single reference survey
#' contains all the variables relevant to participation. An optional cumulative
#' pre-calibration step aligns overlapping auxiliary information across reference
#' surveys before the main estimation.
#'
#' Variance estimation uses Taylor linearization, providing an analytic variance
#' estimator that accounts for both the estimation of the pseudo-weights and the
#' complex sampling designs of the reference surveys through integration with the
#' \pkg{survey} package.
#'
#' @section Typical workflow:
#' Estimation proceeds in two steps:
#' \enumerate{
#'   \item \code{\link{est_pw}()} estimates the pseudo-weights from the
#'     nonprobability sample and one or more reference survey design objects, and
#'     stores the internal quantities needed for variance estimation. It does not
#'     require an outcome variable.
#'   \item \code{\link{pwmean}()} takes the object returned by \code{est_pw()}
#'     and estimates the pseudo-weighted mean (or prevalence) of an outcome,
#'     overall or within domains, together with its standard error and confidence
#'     interval.
#' }
#' Numerical settings for solving the estimating equations are supplied through
#' \code{\link{pw_solver_control}()}.
#'
#' @section Datasets:
#' The package ships example datasets derived from real studies:
#' \code{\link{sc}} (a nonprobability sample), \code{\link{sp1}} and
#' \code{\link{sp2}} (probability reference surveys), and
#' \code{\link{sp1_bootstrap}} (replicate weights for \code{sp1}). The package
#' vignette works through both a one-reference and a multi-reference analysis.
#'
#' @references
#' Wang, L., Valliant, R., and Li, Y. (2021).
#' Adjusted logistic propensity weighting methods for population inference
#' using nonprobability volunteer-based epidemiologic cohorts.
#' \emph{Statistics in Medicine}, 40(24), 5237--5250.
#' doi:10.1002/sim.9122
#'
#' Chen, Y., Li, P., and Wu, C. (2020).
#' Doubly robust inference with nonprobability survey samples.
#' \emph{Journal of the American Statistical Association},
#' 115(532), 2011--2021.
#' doi:10.1080/01621459.2019.1677241
#'
#' Landsman, V., Wang, L., Carrillo-Garcia, I., Mitani, A. A.,
#' Smith, P. M., Graubard, B. I., Bui, T., and Carnide, N. (2026).
#' Correction for Participation Bias in Nonprobability Samples
#' Using Multiple Reference Surveys.
#' \emph{Statistics in Medicine}, 45(3--5).
#' doi:10.1002/SIM.70403
#'
#' @seealso
#' \code{\link{est_pw}}, \code{\link{pwmean}}, \code{\link{pw_solver_control}}
#'
"_PACKAGE"
