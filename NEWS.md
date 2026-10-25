# nonprobsampling (development version)

* Fixed missing-value handling for character domains in `pwmean()`: empty
  and whitespace-only strings are now detected before applying `na.action`
  in the pseudo-weighted and unweighted estimation paths. Omitted row
  indices include these values, and `na.fail` rejects them.

* Corrected the `sp1_bootstrap` help example to use
  `combined.weights = TRUE`, because the bootstrap replicate weights
  already include the main survey weight.

* Added a `confint()` method for `pwmean` objects, inherited by
  `pwmean_factor`, providing normal-approximation confidence intervals
  with configurable confidence levels and estimate group selection.

* Added a `coef()` method for `pwmean` objects to extract pseudo-weighted
  means. `pwmean_factor` objects inherit the method and return category
  proportions (prevelance), named by category and domain.

* Added a `coef()` method for `pw_fit` objects to extract participation
  model coefficients.

# nonprobsampling 0.1.0

* Initial CRAN release.
