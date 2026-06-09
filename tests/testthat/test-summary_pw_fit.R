# test-summary_pw_fit.R
# Tests for the summary.pw_fit S3 method (full call / method / solver
# diagnostics / coefficient table / NA summary).


# Setup ----

data(sc)
data(sp1)
data(sp2)

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

test_that("summary.pw_fit prints call, method, diagnostics and coefficients", {
  expect_output(summary(fit_cali), "Call:",                            fixed = TRUE)
  expect_output(summary(fit_cali), "Method: One reference calibration", fixed = TRUE)
  expect_output(summary(fit_cali), "Solver diagnostics:",              fixed = TRUE)
  expect_output(summary(fit_cali), "Participation model coefficients:", fixed = TRUE)
})

test_that("summary.pw_fit returns its argument invisibly", {
  txt <- capture.output(rv <- withVisible(summary(fit_cali)))
  expect_false(rv$visible)
  expect_identical(rv$value, fit_cali)
})


# Method labels ----

test_that("summary.pw_fit labels one-reference ALP", {
  expect_output(summary(fit_alp), "Method: One reference ALP", fixed = TRUE)
})

test_that("summary.pw_fit labels one-reference CLW", {
  expect_output(summary(fit_clw), "Method: One reference CLW", fixed = TRUE)
})

test_that("summary.pw_fit labels multi-reference calibration", {
  expect_output(summary(fit_multi), "Multi-reference calibration", fixed = TRUE)
})


# Solver diagnostics block ----

test_that("summary.pw_fit prints solver name and termination code", {
  expect_output(summary(fit_cali), "Solver:",           fixed = TRUE)
  expect_output(summary(fit_cali), "Termination code:", fixed = TRUE)
  expect_output(summary(fit_cali), "Iterations:",       fixed = TRUE)
})


# NA summary ----

test_that("summary.pw_fit reports the NA deletion note when rows are dropped", {
  sc_na <- sc
  sc_na$agecat[1:6] <- NA
  fit_na <- suppressMessages(est_pw(list(sc_na, des1), method = "calibration",
                                    p_formula = ~ agecat + race, control = ctrl))
  expect_output(summary(fit_na), "deleted due to missingness in sc", fixed = TRUE)
})
