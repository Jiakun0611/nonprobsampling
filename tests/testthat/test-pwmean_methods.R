# test-pwmean_methods.R
# Tests for the S3 methods on pwmean objects:
#   print.pwmean, summary.pwmean, na.action.pwmean.


# Setup ----

data(sc)
data(sp1)

des1 <- survey::svydesign(ids = ~psu_sp1, strata = ~strata_sp1,
                          weights = ~wts_sp1, data = sp1, nest = TRUE)

ctrl <- pw_solver_control(ftol = 1e-6)

fit_cali <- suppressMessages(est_pw(list(sc, des1), method = "calibration",
                                    p_formula = ~ agecat + race, control = ctrl))
fit_alp  <- suppressMessages(est_pw(list(sc, des1), method = "alp",
                                    p_formula = ~ agecat + race, control = ctrl))

out_overall <- pwmean(fit_cali, y = "psa_level")
out_race    <- pwmean(fit_cali, y = "psa_level", zcol = "race")
out_factor  <- pwmean(fit_cali, y = "BMI")


# print.pwmean ----

test_that("print.pwmean shows the estimator header and per-domain fields", {
  expect_output(print(out_overall), "Pseudo-weighted", fixed = TRUE)
  expect_output(print(out_overall), "Estimators:",     fixed = TRUE)
  expect_output(print(out_overall), "Domain:",         fixed = TRUE)
  expect_output(print(out_overall), "Mean:",           fixed = TRUE)
  expect_output(print(out_overall), "Std. Error:",     fixed = TRUE)
  expect_output(print(out_overall), "95% CI:",         fixed = TRUE)
})

test_that("print.pwmean prints one 'Domain:' block per domain level", {
  out <- capture.output(print(out_race))
  expect_equal(sum(grepl("Domain:", out, fixed = TRUE)), nrow(out_race$estimates))
})

test_that("print.pwmean uppercases ALP in the header", {
  out_alp <- pwmean(fit_alp, y = "psa_level")
  expect_output(print(out_alp), "Pseudo-weighted (ALP)", fixed = TRUE)
})

test_that("print.pwmean returns its argument invisibly", {
  txt <- capture.output(rv <- withVisible(print(out_overall)))
  expect_false(rv$visible)
  expect_identical(rv$value, out_overall)
})

test_that("print.pwmean_factor uses category and prevalence labels", {
  expect_s3_class(out_factor, "pwmean_factor")

  out <- capture.output(print(out_factor))

  expect_true(any(grepl("Category:", out, fixed = TRUE)))
  expect_true(any(grepl("Domain:", out, fixed = TRUE)))
  expect_true(any(grepl("Prevalence:", out, fixed = TRUE)))
  expect_false(any(grepl("Mean:", out, fixed = TRUE)))
})


# summary.pwmean ----

test_that("summary.pwmean shows call, method and estimator sections (overall)", {
  expect_output(summary(out_overall), "Call:",                            fixed = TRUE)
  expect_output(summary(out_overall), "Method: One reference calibration", fixed = TRUE)
  expect_output(summary(out_overall), "Domain: Overall",                  fixed = TRUE)
  expect_output(summary(out_overall), "Unweighted estimators:",           fixed = TRUE)
  expect_output(summary(out_overall), "Pseudo-weighted",                  fixed = TRUE)
})

test_that("summary.pwmean prints a multi-row table for a factor domain", {
  expect_output(summary(out_race), "Unweighted estimators:", fixed = TRUE)
  expect_output(summary(out_race), "domain",                 fixed = TRUE)
})

test_that("summary.pwmean returns its argument invisibly", {
  txt <- capture.output(rv <- withVisible(summary(out_overall)))
  expect_false(rv$visible)
  expect_identical(rv$value, out_overall)
})

test_that("summary.pwmean_factor uses category and prevalence labels", {
  out <- capture.output(summary(out_factor))

  expect_true(any(grepl("category", out, fixed = TRUE)))
  expect_true(any(grepl("domain", out, fixed = TRUE)))
  expect_true(any(grepl("prevalence", out, fixed = TRUE)))
  expect_false(any(grepl("Mean:", out, fixed = TRUE)))
  expect_false(any(grepl(" mean ", out, fixed = TRUE)))
})


# na.action.pwmean ----

test_that("na.action(pwmean) reports no omitted rows when y has no NA", {
  na_obj <- na.action(out_overall)
  expect_true(is.null(na_obj) || length(na_obj) == 0L)
})

test_that("na.action(pwmean) returns the omitted index object when y has NA", {
  sc_y <- sc
  sc_y$psa_level[1:5] <- NA_real_
  fit_y <- suppressMessages(est_pw(list(sc_y, des1), method = "calibration",
                                   p_formula = ~ agecat + race, control = ctrl))
  out_y <- pwmean(fit_y, y = "psa_level", na.action = stats::na.exclude)
  expect_false(is.null(na.action(out_y)))
})
