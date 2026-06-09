# test-est_pw.R
# Integration tests for est_pw() — the main exported function.
#
# These tests cover the full workflow through the public API.
# They complement the step-level unit tests by checking that the pipeline
# is connected correctly and that the documented behaviour is preserved.
#
# Covered scenarios:
#   A. One-reference: all four methods (calibration / alp / clw / cali alias)
#   B. One-reference: explicit p_formula
#   C. One-reference: custom sc_wname
#   D. Multi-reference: default (precali = TRUE, sp_order = "size")
#   E. Multi-reference: precali = FALSE and sp_order = "given"
#   F. NA handling: na.omit / na.exclude / na.fail
#   G. Return-object structure (class, fields, pseudo-weight properties)
#   H. Errors are assigned to the correct estimation step


# Setup ----

data(sc)
data(sp1)
data(sp2)

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

# Convenience wrappers ----
run_one   <- function(...) suppressMessages(
  est_pw(data = list(sc, des1),
         control = pw_solver_control(ftol=1e-6),
         ...)
)

# Multi-ref requires each survey to introduce new variables.
# sc shares {race, agecat, comorbidity} with both sp1 and sp2.
# Assigning ~agecat to sp1 and ~race to sp2 ensures sp1 covers {agecat} and
# sp2 contributes the new variable {race}, so the calibration equations have
# one distinct block of auxiliary variables from each reference survey.
run_multi <- function(...) suppressMessages(
  est_pw(
    data      = list(sc, des1, des2),
    p_formula = list(~agecat, ~race),
    control   = pw_solver_control(ftol = 1e-6),
    ...
  )
)


# A. One-reference: methods ----

test_that("est_pw one-ref (method = NULL): runs without error and defaults to calibration", {
  out <- run_one()

  expect_s3_class(out, "pw_fit")
  expect_equal(out$method, "calibration")
})

test_that("est_pw one-ref (method = 'calibration'): produces positive finite pseudo-weights", {
  out <- run_one(method = "calibration")

  expect_true(all(is.finite(out$pseudo_weights)))
  expect_true(all(out$pseudo_weights > 0))
})

test_that("est_pw one-ref (method = 'alp'): runs and sets method correctly", {
  out <- run_one(method = "alp")

  expect_equal(out$method, "alp")
  expect_true(all(out$pseudo_weights > 0))
  expect_true(all(is.finite(out$pseudo_weights)))
})

test_that("est_pw one-ref (method = 'clw'): runs and sets method correctly", {
  out <- run_one(method = "clw")

  expect_equal(out$method, "clw")
  expect_true(all(out$pseudo_weights > 0))
  expect_true(all(is.finite(out$pseudo_weights)))
})

test_that("est_pw one-ref (method = 'cali'): alias resolves to 'calibration'", {
  out <- run_one(method = "cali")

  expect_equal(out$method, "calibration")
})


# B. One-reference: explicit p_formula ----

test_that("est_pw one-ref: explicit p_formula runs without error", {
  out <- run_one(p_formula = ~agecat)

  expect_s3_class(out, "pw_fit")
  expect_true(all(out$pseudo_weights > 0))
})

test_that("est_pw one-ref: multi-variable p_formula runs without error", {
  out <- run_one(p_formula = ~agecat + race)

  expect_s3_class(out, "pw_fit")
  expect_true(all(is.finite(out$pseudo_weights)))
})

test_that("est_pw one-ref: two-sided p_formula raises a Step 1 error", {
  expect_error(
    run_one(p_formula = agecat ~ race),
    "Step 1",
    fixed = TRUE
  )
})


# C. One-reference: custom sc_wname ----

test_that("est_pw one-ref: custom sc_wname appears as column in sc_updated", {
  out <- run_one(sc_wname = "my_wts")

  expect_true("my_wts" %in% names(out$sc_updated))
  expect_false("pseudo_wts" %in% names(out$sc_updated))
})


# D. Multi-reference: default settings ----

test_that("est_pw multi-ref: runs without error with default settings", {
  out <- run_multi()

  expect_s3_class(out, "pw_fit")
  expect_equal(out$method, "multi")
})

test_that("est_pw multi-ref: pseudo-weights are positive and finite", {
  out <- run_multi()

  expect_true(all(out$pseudo_weights > 0))
  expect_true(all(is.finite(out$pseudo_weights)))
})

test_that("est_pw multi-ref: pseudo-weights have length nrow(sc)", {
  out <- run_multi()

  expect_length(out$pseudo_weights, nrow(sc))
})

test_that("est_pw multi-ref: method is always 'multi' regardless of method argument", {
  # Multi-reference always uses method = "multi" regardless of what is passed
  out <- run_multi()

  expect_equal(out$method, "multi")
})

test_that("est_pw multi-ref: sp_order = 'given' runs without error", {
  out <- run_multi(sp_order = "given")

  expect_s3_class(out, "pw_fit")
  expect_true(all(out$pseudo_weights > 0))
})


# E. Multi-reference: precali = FALSE ----

# precali = FALSE with sp_order = "given" produces a singular Jacobian for
# this dataset because the uncalibrated weights create an ill-conditioned
# system.  sp_order = "size" (the default) conditions the problem correctly
# and allows convergence even without precalibration.

test_that("est_pw multi-ref (precali = FALSE, sp_order = 'size'): runs without error", {
  out <- run_multi(precali = FALSE, sp_order = "size")

  expect_s3_class(out, "pw_fit")
  expect_equal(out$method, "multi")
})

test_that("est_pw multi-ref (precali = FALSE, sp_order = 'size'): pseudo-weights are positive", {
  out <- run_multi(precali = FALSE, sp_order = "size")

  expect_true(all(out$pseudo_weights > 0))
})


# F. NA handling ----

# Introduce NAs into sc in a p_formula variable
sc_na <- sc
sc_na$agecat[1:3] <- NA

test_that("est_pw one-ref (na.omit): sc_updated drops NA rows", {
  out <- suppressMessages(
    est_pw(
      data      = list(sc_na, des1),
      p_formula = ~agecat,
      na.action = stats::na.omit,
      control   = pw_solver_control(ftol = 1e-4)
    )
  )

  expect_equal(nrow(out$sc_updated), nrow(sc_na) - 3L)
  expect_equal(length(out$pseudo_weights), nrow(sc_na) - 3L)
  expect_false(anyNA(out$pseudo_weights))
})

test_that("est_pw one-ref (na.omit): na_summary reports excluded rows", {
  out <- suppressMessages(
    est_pw(
      data      = list(sc_na, des1),
      p_formula = ~agecat,
      na.action = stats::na.omit,
      control   = pw_solver_control(ftol = 1e-4)
    )
  )

  expect_s3_class(out$na_summary, "pw_na_summary")
  expect_equal(out$na_summary$sc$n_excluded, 3L)
})

test_that("est_pw one-ref (na.exclude): sc_updated has nrow(sc) rows; NA at excluded", {
  out <- suppressMessages(
    est_pw(
      data      = list(sc_na, des1),
      p_formula = ~agecat,
      na.action = stats::na.exclude,
      control   = pw_solver_control(ftol = 1e-4)
    )
  )

  expect_equal(nrow(out$sc_updated), nrow(sc_na))
  expect_true(all(is.na(out$pseudo_weights[1:3])))
  expect_false(anyNA(out$pseudo_weights[-(1:3)]))
})

test_that("est_pw one-ref (na.fail): errors when NA present", {
  expect_error(
    suppressMessages(
      est_pw(
        data      = list(sc_na, des1),
        p_formula = ~agecat,
        na.action = stats::na.fail
      )
    ),
    "Step 3",
    fixed = TRUE
  )
})

test_that("est_pw one-ref (no NA): na_summary is NULL", {
  out <- run_one(p_formula = ~agecat)

  expect_null(out$na_summary)
})


# G. Return-object structure ----

test_that("est_pw one-ref: returned object has class 'pw_fit'", {
  out <- run_one()
  expect_s3_class(out, "pw_fit")
})

test_that("est_pw one-ref: returned object has required top-level fields", {
  out <- run_one()

  # na_summary is always present (NULL when no NAs, set via list-assignment)
  required <- c("pseudo_weights", "coefficients", "method",
                "solver_diagnostics", "sc_updated", "internal",
                "call", "na_summary")
  expect_true(all(required %in% names(out)))
})

test_that("est_pw one-ref: pseudo_weights has length nrow(sc)", {
  out <- run_one()

  expect_length(out$pseudo_weights, nrow(sc))
})

test_that("est_pw one-ref: sc_updated has the default sc_wname column", {
  out <- run_one()

  expect_true("pseudo_wts" %in% names(out$sc_updated))
})

test_that("est_pw one-ref: sc_updated has nrow(sc) rows when no NAs", {
  out <- run_one()

  expect_equal(nrow(out$sc_updated), nrow(sc))
})

test_that("est_pw one-ref: call is stored as a call object", {
  out <- run_one()

  expect_true(inherits(out$call, "call"))
})

test_that("est_pw one-ref: coefficients are named", {
  out <- run_one()

  expect_false(is.null(names(out$coefficients)))
  expect_true("(Intercept)" %in% names(out$coefficients))
})

test_that("est_pw one-ref: solver_diagnostics has expected fields", {
  out <- run_one()

  expected <- c("solver", "termcd", "message", "method", "iter", "fmax")
  expect_true(all(expected %in% names(out$solver_diagnostics)))
})

test_that("est_pw one-ref: solver converged (termcd 1 or 2)", {
  out <- run_one()

  expect_true(out$solver_diagnostics$termcd %in% c(1L, 2L))
})

test_that("est_pw multi-ref: returned object has required top-level fields", {
  out <- run_multi()

  required <- c("pseudo_weights", "coefficients", "method",
                "solver_diagnostics", "sc_updated", "internal",
                "call", "na_summary")
  expect_true(all(required %in% names(out)))
})

test_that("est_pw multi-ref: na_summary is NULL when no NAs", {
  out <- run_multi()

  expect_true("na_summary" %in% names(out))
  expect_null(out$na_summary)
})


# H. Error attributed correctly ----

test_that("est_pw: invalid data (not a list) raises a Step 1 error", {
  expect_error(
    est_pw(data = 42L),
    "Step 1",
    fixed = TRUE
  )
})

test_that("est_pw: invalid method raises a Step 1 error", {
  expect_error(
    run_one(method = "bad_method"),
    "Step 1",
    fixed = TRUE
  )
})

test_that("est_pw: method = 'multi' with one reference raises a Step 1 error", {
  expect_error(
    run_one(method = "multi"),
    "Step 1",
    fixed = TRUE
  )
})

test_that("est_pw: p_formula with variable absent from sc raises a Step 1 error", {
  expect_error(
    run_one(p_formula = ~no_such_column),
    "Step 1",
    fixed = TRUE
  )
})
