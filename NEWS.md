# nonprobsampling (development version)

* Added a `confint()` method for `pwmean` objects, inherited by
  `pwmean_factor`, providing normal-approximation confidence intervals
  with configurable confidence levels and estimate selection.

* Added a `coef()` method for `pwmean` objects to extract pseudo-weighted
  means. `pwmean_factor` objects inherit the method and return category
  proportions (prevelance), named by category and domain.

* Added a `coef()` method for `pw_fit` objects to extract participation
  model coefficients.

# nonprobsampling 0.1.0

* Initial CRAN release.
