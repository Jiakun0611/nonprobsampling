# nonprobsampling

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nonprobsampling)](https://CRAN.R-project.org/package=nonprobsampling)
[![pkgdown site](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://jiakun0611.github.io/nonprobsampling/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**Inference for Nonprobability Samples Using Multiple Reference Surveys**

`nonprobsampling` 
implements pseudo-weighting methods for finite population inference from nonprobability samples, such as convenience samples, volunteer cohorts, and opt-in panels.
Because the participation mechanism in a
nonprobability sample is unknown, unadjusted estimates of population means and
prevalences may be biased. The package addresses this issue by leveraging auxiliary information from one or multiple probability reference surveys to estimate participation probabilities and using their inverses as pseudo-weights to obtain bias-corrected estimates of finite population means and prevalences. 

The implemented methods are based on the generalized estimating equations framework of Landsman et al. (2026). This includes a multi-reference extension that enables the integration of auxiliary information across multiple reference surveys.

## Installation

Install the released version from CRAN (once available):

```r
install.packages("nonprobsampling")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Jiakun0611/nonprobsampling")
```

## Methods

With **one** reference survey:

- **Calibration** — raking ratio calibration (Landsman et al., 2026)
- **ALP** — adjusted logistic propensity weighting (Wang, Valliant, and Li, 2021)
- **CLW** — Chen, Li, and Wu (2020)


With **multiple** reference surveys:

- **Multi-reference calibration** — enables the integration of auxiliary information across multiple surveys when no single reference survey contains all variables relevant to participation (Landsman et al., 2026), with an optional cumulative precalibration step to preserve information on the relationships between overlapping and unique auxiliary variables across reference surveys.

Variance estimation is based on Taylor linearization, with complex sampling designs in the reference surveys handled through integration with the
[`survey`](https://CRAN.R-project.org/package=survey) package; when bootstrap replicate weights are provided, bootstrap-based variance estimation is also supported.

## Usage

Estimation proceeds in two steps: `est_pw()` estimates pseudo-weights, then
`pwmean()` estimates a pseudo-weighted mean or prevalence for an outcome.

```r
library(nonprobsampling)

data(sc)    # nonprobability sample (outcome: psa_level)
data(sp1)   # probability reference survey

# Reference survey design
ref1_design <- survey::svydesign(
  ids     = ~psu_sp1,
  strata  = ~strata_sp1,
  weights = ~wts_sp1,
  data    = sp1,
  nest    = TRUE
)

# Step 1: estimate pseudo-weights (one-reference calibration)
fit <- est_pw(
  data      = list(sc, ref1_design),
  p_formula = ~ agecat + race + education + comorbidity + BMI + diabetes,
  method    = "calibration",
  control   = pw_solver_control(ftol = 1e-6)
)
print(fit)
summary(fit)

# Step 2: pseudo-weighted mean of the outcome, by BMI categories
out <- pwmean(fit, y = "psa_level", zcol = "BMI")
print(out)
summary(out)
```

With multiple reference surveys, users provide one survey design object for each reference survey and a corresponding list of participation model formulas, with one formula specified for each survey.

```r
data(sp2)   # second probability reference survey

ref2_design <- survey::svydesign(
  ids     = ~psu_sp2,
  strata  = ~strata_sp2,
  weights = ~wts_sp2,
  data    = sp2,
  nest    = TRUE
)

fit2 <- est_pw(
  data = list(sc, ref1_design, ref2_design),
  p_formula = list(
    ~ agecat + race + education + psa_level + pros_enlarged + comorbidity,
    ~ agecat + race + BMI + diabetes + comorbidity
  ),
  sp_order = "size",
  precali = TRUE,
  control = pw_solver_control(ftol = 1e-6)
)
print(fit2)
summary(fit2)
```

See `vignette("nonprobsampling")` for more details.

## Datasets

The package includes example datasets used throughout the documentation:

| Dataset         | Description                                              |
|-----------------|----------------------------------------------------------|
| `sc`            | Nonprobability sample (synthetic, NHANES-based)          |
| `sp1`           | First probability reference survey (NHANES 1999–2010)    |
| `sp2`           | Second probability reference survey (NHIS 1997–2008)     |
| `sp1_bootstrap` | `sp1` with bootstrap replicate weights                   |

## Getting help

Click any topic to open its help page (or run the command in R):

<div style="font-family: monospace; line-height: 1.6; padding: 1em; border-radius: 6px; overflow-x: auto;">
<span style="color:#6a737d;">## Package overview</span><br>
?<a href="reference/nonprobsampling-package.html">nonprobsampling</a><br>
<br>
<span style="color:#6a737d;">## Main functions</span><br>
?<a href="reference/est_pw.html">est_pw</a><br>
?<a href="reference/pwmean.html">pwmean</a><br>
?<a href="reference/pw_solver_control.html">pw_solver_control</a><br>
<br>
<span style="color:#6a737d;">## Datasets</span><br>
?<a href="reference/sc.html">sc</a><br>
?<a href="reference/sp1.html">sp1</a><br>
?<a href="reference/sp2.html">sp2</a><br>
?<a href="reference/sp1_bootstrap.html">sp1_bootstrap</a><br>
<br>
<span style="color:#6a737d;">## Vignette</span><br>
<a href="articles/nonprobsampling.html">vignette("nonprobsampling")</a>
</div>

## Citation

To cite the package in publications, run:

```r
citation("nonprobsampling")
```

## References

- Chen, Y., Li, P., and Wu, C. (2020). Doubly robust inference with
  nonprobability survey samples. *Journal of the American Statistical
  Association*, 115(532), 2011–2021.
  [doi:10.1080/01621459.2019.1677241](https://doi.org/10.1080/01621459.2019.1677241)
- Landsman, V., Wang, L., Carrillo-Garcia, I., Mitani, A. A., Smith, P. M.,
  Graubard, B. I., Bui, T., and Carnide, N. (2026). Correction for
  participation bias in nonprobability samples using multiple reference
  surveys. *Statistics in Medicine*, 45(3–5).
  [doi:10.1002/sim.70403](https://doi.org/10.1002/sim.70403)
- Wang, L., Valliant, R., and Li, Y. (2021). Adjusted logistic propensity
  weighting methods for population inference using nonprobability
  volunteer-based epidemiologic cohorts. *Statistics in Medicine*, 40(24),
  5237–5250.
  [doi:10.1002/sim.9122](https://doi.org/10.1002/sim.9122)

## License

GPL-3 © Jiakun Lin, Victoria Landsman, Aya A. Mitani
