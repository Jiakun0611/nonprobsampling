# test-print_pw_fit.R
# Tests for the print.pw_fit S3 method (compact overview of a pw_fit object)
# and na.action.pw_fit.
#
# Output content is checked by matching stable structural labels, not exact
# numbers, so the tests do not break when the bundled datasets change.


# Setup ----

data(sc)
data(sp1)
data(sp2)

options(survey.lonely.psu = "adjust")

des1 <- survey::svydesign(ids = ~psu_sp1, strata = ~strata_sp1,
                          weights = ~wts_sp1, data = sp1, nest = TRUE)
des2 <- survey::svydesign(ids = ~psu_sp2, strata = ~strata_sp2,
                          weights = ~wts_sp2, data = sp2, nest = TRUE)

ctrl <- pw_solver_control(ftol = 1e-6)

fit_cali  <- suppressMessages(est_pw(list(sc, des1), method = "calibration",
                                     p_formula = ~ agecat + race, control = ctrl))
fit_alp   <- suppressMessages(est_pw(list(sc, des1), method = "alp",
                                     p_formula = ~ agecat + race, control = ctrl))
fit_clw   <- suppressMessages(est_pw(list(sc, des1), method = "clw",
                                     p_formula = ~ agecat + race, control = ctrl))
fit_multi <- suppressMessages(est_pw(list(sc, des1, des2),
                                     p_formula = list(~ agecat, ~ race), control = ctrl))


# Common sections ----

test_that("print.pw_fit shows header, call, method, model, convergence, weights", {
  expect_output(print(fit_cali), "Pseudo-weight fit",      fixed = TRUE)
  expect_output(print(fit_cali), "Call:",                  fixed = TRUE)
  expect_output(print(fit_cali), "Method:",                fixed = TRUE)
  expect_output(print(fit_cali), "Participation model:",   fixed = TRUE)
  expect_output(print(fit_cali), "Convergence:",           fixed = TRUE)
  expect_output(print(fit_cali), "Pseudo-weights",         fixed = TRUE)
  expect_output(print(fit_cali), "Sum:",                   fixed = TRUE)
  expect_output(print(fit_cali), "summary()",              fixed = TRUE)
})

test_that("print.pw_fit returns its argument invisibly", {
  txt <- capture.output(rv <- withVisible(print(fit_cali)))
  expect_false(rv$visible)
  expect_identical(rv$value, fit_cali)
})


# Method labels ----

test_that("print.pw_fit labels one-reference calibration", {
  expect_output(print(fit_cali), "One reference calibration", fixed = TRUE)
})

test_that("print.pw_fit labels one-reference ALP", {
  expect_output(print(fit_alp), "One reference ALP", fixed = TRUE)
})

test_that("print.pw_fit labels one-reference CLW", {
  expect_output(print(fit_clw), "One reference CLW", fixed = TRUE)
})

test_that("print.pw_fit labels multi-reference calibration", {
  expect_output(print(fit_multi), "Multi-reference calibration", fixed = TRUE)
})


# Participation-model parameter count ----

test_that("print.pw_fit reports the number of participation-model parameters", {
  p <- length(fit_cali$coefficients)
  expect_output(print(fit_cali), sprintf("%d parameters", p), fixed = TRUE)
})

test_that("print.pw_fit notes '(incl. intercept)' for one-reference but not multi", {
  expect_output(print(fit_cali), "(incl. intercept)", fixed = TRUE)

  out <- capture.output(print(fit_multi))
  expect_false(any(grepl("incl. intercept", out, fixed = TRUE)))
})


# Convergence ----

test_that("print.pw_fit reports a converged solver as 'converged'", {
  expect_output(print(fit_cali), "converged", fixed = TRUE)
})


# Pseudo-weight summary ----

test_that("print.pw_fit weight summary reports n and a six-number summary", {
  n <- sum(!is.na(fit_cali$pseudo_weights))
  expect_output(print(fit_cali), sprintf("n = %d", n), fixed = TRUE)
  expect_output(print(fit_cali), "Min.",   fixed = TRUE)
  expect_output(print(fit_cali), "Median", fixed = TRUE)
  expect_output(print(fit_cali), "Max.",   fixed = TRUE)
})


# na.exclude path ----

test_that("print.pw_fit (na.exclude) reports NA weights and a deletion note", {
  sc_na <- sc
  sc_na$agecat[1:6] <- NA
  fit_ex <- suppressMessages(est_pw(list(sc_na, des1), method = "calibration",
                                    p_formula = ~ agecat + race, control = ctrl,
                                    na.action = stats::na.exclude))
  expect_output(print(fit_ex), "NA", fixed = TRUE)
  expect_output(print(fit_ex), "deleted due to missingness in sc", fixed = TRUE)
})


# na.action.pw_fit ----

test_that("na.action(pw_fit) reports no omitted rows for a complete-case fit", {
  na_obj <- na.action(fit_cali)
  expect_true(is.null(na_obj) || length(na_obj) == 0L)
})

test_that("na.action(pw_fit) returns the omitted-row index object when rows drop", {
  sc_na <- sc
  sc_na$agecat[1:6] <- NA
  fit_om <- suppressMessages(est_pw(list(sc_na, des1), method = "calibration",
                                    p_formula = ~ agecat + race, control = ctrl,
                                    na.action = stats::na.omit))
  na_obj <- na.action(fit_om)
  expect_false(is.null(na_obj))
  expect_s3_class(na_obj, "omit")
})
