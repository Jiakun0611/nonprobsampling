#' Probability Reference Sample 1 (sp1)
#'
#' This dataset represents a probability sample derived from the
#' National Health and Nutrition Examination Survey (NHANES).
#' It is used as a probability reference survey to support inverse
#' propensity weighting methods implemented in the \code{nonprobsampling} package.
#'
#' The dataset provides auxiliary variables that overlap with the nonprobability
#' sample \code{sc}, enabling the construction of pseudo-weights via shared
#' covariates. Survey design variables and sampling weights are included to
#' allow design-consistent estimation.
#'
#' @format A data frame with 14 variables:
#' \describe{
#'   \item{agecat}{Age category (factor with 4 levels: 1 = 55--59, 2 = 60--64, 3 = 65--69, 4 = 70+)}
#'   \item{marital}{Marital status (factor with 4 levels: 1 = Married Or Living As Married, 2 = Widowed, 3 = Divorced or Separated, 4 = Never Married)}
#'   \item{race}{Race category (factor with 4 levels: 1 = White, 2 = Black, 3 = Hispanic, 4 = Other)}
#'   \item{education}{Education level (factor with 5 levels: 1 = Less Than 8 Years, 2 = 8--11 Years, 3 = 12 Years Or Completed High School, 4 = College Graduate, 5 = Postgraduate)}
#'   \item{employment}{Employment status (factor with 2 levels: 0 = Not Working, 1 = Working)}
#'   \item{smoking}{Smoking status (factor with 3 levels: 1 = Never Smoker, 2 = Former Smoker, 3 = Current Smoker)}
#'   \item{comorbidity}{General comorbidity indicator (factor with 2 levels: 0 = No, 1 = Yes)}
#'   \item{psa_level}{Serum prostate-specific antigen level (numeric)}
#'   \item{BMI}{Body mass index category (factor with 5 levels: "Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese")}
#'   \item{diabetes}{Diabetes diagnosis indicator (factor with 2 levels: 0 = No, 1 = Yes)}
#'   \item{pros_enlarged}{Prostate enlargement indicator (factor with 2 levels: 0 = No, 1 = Yes)}
#'   \item{strata_sp1}{Stratum identifier for complex survey design (numeric)}
#'   \item{psu_sp1}{Primary sampling unit identifier for complex survey design (numeric)}
#'   \item{wts_sp1}{10-year interview sampling weights (numeric)}
#' }
#'
#' @details
#' The dataset is constructed from NHANES cycles using harmonized variables.
#' The variable \code{psa_level} is the outcome of interest and is also observed
#' in the nonprobability sample \code{sc}, allowing for benchmarking.
#'
#' Survey design variables \code{psu_sp1} and \code{strata_sp1}, together with
#' \code{wts_sp1}, should be used when performing design-based inference with
#' this reference sample.
#'
#' This dataset is intended for use alongside \code{sc} and \code{sp2} to
#' demonstrate bias correction methods for nonprobability samples using
#' one or multiple probability reference surveys.
#'
#' @usage data(sp1)
#'
#' @examples
#' data(sp1)
#' str(sp1)
#' summary(sp1)
"sp1"
