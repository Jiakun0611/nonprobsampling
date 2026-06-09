# test-print_pw_na_summary.R
# Tests for the print.pw_na_summary S3 method. The pw_na_summary object is
# produced by est_pw() and stored in fit$na_summary whenever rows are dropped
# because of missing participation model variables.


# Setup ----

data(sc)
data(sp1)
data(sp2)

des1 <- survey::svydesign(ids = ~psu_sp1, strata = ~strata_sp1,
                          weights = ~wts_sp1, data = sp1, nest = TRUE)
des2 <- survey::svydesign(ids = ~psu_sp2, strata = ~strata_sp2,
                          weights = ~wts_sp2, data = sp2, nest = TRUE)

ctrl <- pw_solver_control(ftol = 1e-6)

# Exclusions by introducing NA in a participation-model variable in sc.
sc_na <- sc
sc_na$agecat[1:6] <- NA

fit_one <- suppressMessages(est_pw(list(sc_na, des1), method = "calibration",
                                   p_formula = ~ agecat + race, control = ctrl))
nas_one <- fit_one$na_summary

fit_multi <- suppressMessages(est_pw(list(sc_na, des1, des2),
                                     p_formula = list(~ agecat, ~ race), control = ctrl))
nas_multi <- fit_multi$na_summary


# Object class ----

test_that("est_pw stores a pw_na_summary object when rows are excluded", {
  expect_s3_class(nas_one, "pw_na_summary")
})


# print.pw_na_summary ----

test_that("print.pw_na_summary prints the header and the count columns", {
  expect_output(print(nas_one), "NA processing summary:", fixed = TRUE)
  expect_output(print(nas_one), "n_orig",                 fixed = TRUE)
  expect_output(print(nas_one), "n_used",                 fixed = TRUE)
  expect_output(print(nas_one), "n_excluded",             fixed = TRUE)
})

test_that("print.pw_na_summary lists sc and the reference survey rows (one-ref)", {
  out <- capture.output(print(nas_one))
  expect_true(any(grepl("^sc", out)))
  expect_true(any(grepl("sp", out)))
})

test_that("print.pw_na_summary returns its argument invisibly", {
  txt <- capture.output(rv <- withVisible(print(nas_one)))
  expect_false(rv$visible)
  expect_identical(rv$value, nas_one)
})

test_that("print.pw_na_summary handles the multi-reference layout", {
  skip_if(is.null(nas_multi), "no exclusions recorded in the multi-reference fit")
  expect_s3_class(nas_multi, "pw_na_summary")
  expect_output(print(nas_multi), "NA processing summary:", fixed = TRUE)
  out <- capture.output(print(nas_multi))
  expect_gte(sum(grepl("sp", out)), 1L)
})
