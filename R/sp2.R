#' Probability Reference Sample 2 (sp2)
#'
#' This dataset represents a probability sample derived from the
#' National Health Interview Survey (NHIS).
#' It is used as a second probability reference survey to support inverse
#' propensity weighting methods implemented in the \code{nonprobsampling} package.
#'
#' The dataset provides auxiliary variables that overlap with the nonprobability
#' sample \code{sc}, enabling the construction of pseudo-weights via shared
#' covariates. Survey design variables and sampling weights are included to
#' allow design-consistent estimation.
#'
#' @format A data frame with 11 variables:
#' \describe{
#'   \item{agecat}{Age category (factor with 4 levels: 1 = 55--59, 2 = 60--64, 3 = 65--69, 4 = 70+)}
#'   \item{marital}{Marital status (factor with 4 levels: 1 = Married Or Living As Married, 2 = Widowed, 3 = Divorced or Separated, 4 = Never Married)}
#'   \item{race}{Race category (factor with 4 levels: 1 = White, 2 = Black, 3 = Hispanic, 4 = Other)}
#'   \item{employment}{Employment status (factor with 2 levels: 0 = Not Working, 1 = Working)}
#'   \item{diabetes}{Diabetes diagnosis indicator (factor with 2 levels: 0 = No, 1 = Yes)}
#'   \item{BMI}{Body mass index category (factor with 5 levels: "Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese")}
#'   \item{smoking}{Smoking status (factor with 3 levels: 1 = Never Smoker, 2 = Former Smoker, 3 = Current Smoker)}
#'   \item{comorbidity}{General comorbidity indicator (factor with 2 levels: 0 = No, 1 = Yes)}
#'   \item{wts_sp2}{Sampling weights (numeric)}
#'   \item{strata_sp2}{Stratum identifier for complex survey design (numeric)}
#'   \item{psu_sp2}{Primary sampling unit identifier for complex survey design (numeric)}
#' }
#'
#' @details
#' The dataset is constructed from NHIS cycles using harmonized variables.
#' Unlike \code{sp1}, this reference sample does not contain \code{psa_level},
#' as the PSA variable is not available in the NHIS. It is therefore used
#' purely as an auxiliary reference for covariate distribution calibration.
#'
#' Survey design variables \code{psu_sp2} and \code{strata_sp2}, together with
#' \code{wts_sp2}, should be used when performing design-based inference with
#' this reference sample.
#'
#' This dataset is intended for use alongside \code{sc} and \code{sp1} to
#' demonstrate bias correction methods for nonprobability samples using
#' multiple probability reference surveys.
#'
#' @usage data(sp2)
#'
#' @examples
#' data(sp2)
#' str(sp2)
#' summary(sp2)
"sp2"
