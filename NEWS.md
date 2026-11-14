# nonprobsampling 0.2.0

## New features

* Added `coef()` methods to extract participation model coefficients from
  `pw_fit` objects and pseudo-weighted means or category prevalence from
  `pwmean` objects.

* Added `confint()` for `pwmean` objects, with a choice of confidence level
  and estimates to include.

## Fixes and behavior changes

* Solver diagnostics in `print()`, `summary()`, and verbose output now
  report why the solver stopped and whether the final equation residuals
  satisfy `ftol`.

* Fixed standard errors and confidence intervals when reference survey columns
  collide with internal names such as `h_x`.

* Fixed cumulative precalibration when auxiliary variables are shared by only
  some reference surveys. It also handles factor levels containing spaces or
  hyphens and auxiliary variables named `.w`.

* `est_pw()` now rejects mismatched categories for auxiliary variables stored
  as character in both samples, even when the category counts match.

* `pwmean()` now treats empty or whitespace-only strings in character
  domains as missing before applying `na.action`.

## Documentation

* Added guidance on standardizing continuous auxiliary variables and
  harmonizing categorical coding across samples.

* Corrected the `sp1_bootstrap` help example to use `combined.weights = TRUE`
  because the replicate weights already include the main survey weight.

# nonprobsampling 0.1.0

* Initial CRAN release.
