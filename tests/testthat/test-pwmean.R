# test-pwmean.R
# Integration tests for pwmean() — the main exported function.
#
# These tests cover the full workflow through the public API.
# They complement the step-level unit tests by checking that the pipeline
# is connected correctly and that the documented behaviour is preserved.
#
# Covered scenarios:
#   A. Domain modes: overall / binary / factor
#   B. Methods: calibration / alp / clw / multi all produce valid pwmean objects
#   C. Return-object structure (class, fields, domains data.frame layout)
#   D. CI invariants: lower < mean < upper; width = 2 * z975 * se
#   E. NA handling: na.omit / na.exclude
#   F. Errors are tagged with the correct pipeline step


# Setup ----

data(sc)
data(sp1)
data(sp2)

options(survey.lonely.psu = "adjust")

des1 <- survey::svydesign(
  ids     = ~psu_sp1,
  strata  = ~strata_sp1,
  weights = ~wts_sp1,
  data    = sp1,
  nest    = TRUE
)

des2 <- survey::svydesign(
  ids     = ~psu_sp2,
  strata  = ~strata_sp2,
  weights = ~wts_sp2,
  data    = sp2,
  nest    = TRUE
)

# Build the four pw_fit variants once and reuse across tests.
fit_cali  <- suppressMessages(est_pw(list(sc, des1), method = "calibration",
                                     control = pw_solver_control(ftol = 1e-6)))
fit_alp   <- suppressMessages(est_pw(list(sc, des1), method = "alp",
                                     control = pw_solver_control(ftol = 1e-6)))
fit_clw   <- suppressMessages(est_pw(list(sc, des1), method = "clw",
                                     control = pw_solver_control(ftol = 1e-6)))
fit_multi <- suppressMessages(est_pw(
  data      = list(sc, des1, des2),
  p_formula = list(~agecat, ~race),
  control   = pw_solver_control(ftol = 1e-6)
))

z975 <- stats::qnorm(0.975)

DOM_COLS <- c(
  "domain",
  "unweighted_mean", "unweighted_se", "unweighted_lower", "unweighted_upper",
  "adjusted_mean",   "adjusted_se",   "adjusted_lower",   "adjusted_upper"
)


# A. Domain modes ----

test_that("pwmean overall (zcol = NULL): returns a single-row pwmean object", {
  out <- pwmean(fit_cali, y = "psa_level")

  expect_s3_class(out, "pwmean")
  expect_equal(nrow(out$domains), 1L)
  expect_equal(out$domains$domain, "Overall")
})

test_that("pwmean binary zcol: returns a single-row domain with 'name = 1' label", {
  sc_bin            <- sc
  sc_bin$has_diab   <- as.integer(sc_bin$diabetes == "1")
  fit_bin           <- suppressMessages(est_pw(
    list(sc_bin, des1), method = "calibration",
    control = pw_solver_control(ftol = 1e-6)
  ))
  out <- pwmean(fit_bin, y = "psa_level", zcol = "has_diab")

  expect_s3_class(out, "pwmean")
  expect_equal(nrow(out$domains), 1L)
  expect_equal(out$domains$domain, "has_diab = 1")
})

test_that("pwmean factor zcol (race, 4 levels): returns 4 domain rows in factor-level order", {
  out <- pwmean(fit_cali, y = "psa_level", zcol = "race")

  expect_equal(nrow(out$domains), 4L)
  expect_equal(out$domains$domain, levels(sc$race))
})


# B. Methods ----

test_that("pwmean works for all four methods (calibration / alp / clw / multi)", {
  for (fit in list(fit_cali, fit_alp, fit_clw, fit_multi)) {
    out <- pwmean(fit, y = "psa_level")
    expect_s3_class(out, "pwmean")
    expect_equal(out$method, fit$method)
  }
})

test_that("pwmean: adjusted means are finite for every method", {
  for (fit in list(fit_cali, fit_alp, fit_clw, fit_multi)) {
    out <- pwmean(fit, y = "psa_level")
    expect_true(all(is.finite(out$domains$adjusted_mean)))
    expect_true(all(is.finite(out$domains$adjusted_se)))
    expect_true(all(out$domains$adjusted_se >= 0))
  }
})


# C. Return-object structure ----

test_that("pwmean object has fields: method, domains, na.action, call", {
  out <- pwmean(fit_cali, y = "psa_level")

  expect_named(out, c("method", "domains", "na.action", "call"))
})

test_that("pwmean$call is a captured call to pwmean()", {
  out <- pwmean(fit_cali, y = "psa_level")

  expect_true(is.call(out$call))
  expect_equal(as.character(out$call[[1L]]), "pwmean")
})

test_that("pwmean$domains has all nine documented columns in order", {
  out <- pwmean(fit_cali, y = "psa_level", zcol = "race")

  expect_s3_class(out$domains, "data.frame")
  expect_equal(names(out$domains), DOM_COLS)
})

test_that("pwmean$method propagates from the input pw_fit", {
  expect_equal(pwmean(fit_cali,  y = "psa_level")$method, "calibration")
  expect_equal(pwmean(fit_alp,   y = "psa_level")$method, "alp")
  expect_equal(pwmean(fit_clw,   y = "psa_level")$method, "clw")
  expect_equal(pwmean(fit_multi, y = "psa_level")$method, "multi")
})


# D. CI invariants ----

test_that("pwmean CIs are symmetric: width equals 2 * qnorm(0.975) * se", {
  out <- pwmean(fit_cali, y = "psa_level", zcol = "race")
  d   <- out$domains

  expect_equal(d$adjusted_upper   - d$adjusted_lower,   2 * z975 * d$adjusted_se)
  expect_equal(d$unweighted_upper - d$unweighted_lower, 2 * z975 * d$unweighted_se)
})

test_that("pwmean CIs bracket the point estimates (lower <= mean <= upper)", {
  out <- pwmean(fit_cali, y = "psa_level", zcol = "race")
  d   <- out$domains

  expect_true(all(d$adjusted_lower   <= d$adjusted_mean))
  expect_true(all(d$adjusted_mean    <= d$adjusted_upper))
  expect_true(all(d$unweighted_lower <= d$unweighted_mean))
  expect_true(all(d$unweighted_mean  <= d$unweighted_upper))
})


# E. NA handling ----

test_that("pwmean na.omit: drops missing-y rows silently and proceeds", {
  sc_na              <- sc
  sc_na$psa_level[1:5] <- NA_real_
  fit_na             <- suppressMessages(est_pw(
    list(sc_na, des1), method = "calibration",
    control = pw_solver_control(ftol = 1e-6)
  ))
  out <- pwmean(fit_na, y = "psa_level", na.action = stats::na.omit)

  expect_s3_class(out, "pwmean")
  expect_true(is.finite(out$domains$adjusted_mean))
})

test_that("pwmean na.exclude: produces a valid pwmean object", {
  sc_na              <- sc
  sc_na$psa_level[1:5] <- NA_real_
  fit_na             <- suppressMessages(est_pw(
    list(sc_na, des1), method = "calibration",
    control = pw_solver_control(ftol = 1e-6)
  ))
  out <- pwmean(fit_na, y = "psa_level", na.action = stats::na.exclude)

  expect_s3_class(out, "pwmean")
  expect_true(is.finite(out$domains$adjusted_mean))
})


# F. Step error tagging ----
#
# pwmean() wraps each pipeline step in tryCatch and prefixes the failing
# step's error with "Step N (<fn>) failed:". These tests verify that an
# error raised inside a step is correctly tagged with its step number.

test_that("pwmean tags Step 1 errors with 'Step 1 (input check) failed:'", {
  expect_error(
    pwmean(list(), y = "psa_level"),
    "Step 1 (input check) failed:",
    fixed = TRUE
  )
})

test_that("pwmean tags Step 1 errors when y is not in the convenience sample", {
  expect_error(
    pwmean(fit_cali, y = "not_a_column"),
    "Step 1 (input check) failed:",
    fixed = TRUE
  )
})
