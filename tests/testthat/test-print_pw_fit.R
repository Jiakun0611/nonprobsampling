# test-print_pw_fit.R
# Tests for the print.pw_fit S3 method and na.action.pw_fit.
#
# The output checks use stable labels rather than exact numeric values,
# since example data may change over time.


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

test_that("print.pw_fit shows header, call, method, model, convergence, weights", {
  expect_output(print(fit_cali), "Pseudo-weight fit",      fixed = TRUE)
  expect_output(print(fit_cali), "Call:",                  fixed = TRUE)
  expect_output(print(fit_cali), "Method:",                fixed = TRUE)
  expect_output(print(fit_cali), "Participation model:",   fixed = TRUE)
  expect_output(print(fit_cali), "Termination reason:",    fixed = TRUE)
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
  expect_output(print(fit_cali), "One-reference calibration", fixed = TRUE)
})

test_that("print.pw_fit labels one-reference ALP", {
  expect_output(print(fit_alp), "One-reference ALP", fixed = TRUE)
})

test_that("print.pw_fit labels one-reference CLW", {
  expect_output(print(fit_clw), "One-reference CLW", fixed = TRUE)
})

test_that("print.pw_fit labels multi-reference calibration", {
  expect_output(print(fit_multi), "Multi-reference calibration", fixed = TRUE)
})


# Participation model parameter count ----

test_that("print.pw_fit reports the number of participation model parameters", {
  p <- length(fit_cali$coefficients)
  expect_output(print(fit_cali), sprintf("%d parameters", p), fixed = TRUE)
})

test_that("print.pw_fit notes '(incl. intercept)' for one-reference but not multi", {
  expect_output(print(fit_cali), "(incl. intercept)", fixed = TRUE)

  out <- capture.output(print(fit_multi))
  expect_false(any(grepl("incl. intercept", out, fixed = TRUE)))
})


# Convergence ----

test_that("print.pw_fit reports whether the equation tolerance was met", {
  expect_output(print(fit_cali), "Equation tolerance: Met", fixed = TRUE)
})

test_that("fit diagnostics use the same solver residual as the acceptance check", {
  testthat::skip_if_not("local_mocked_bindings" %in% getNamespaceExports("testthat"),
                        "This test requires testthat::local_mocked_bindings()")
  code <- 2L
  residual <- 1e-6
  testthat::local_mocked_bindings(
    nleqslv = function(...) {
      list(x = fit_cali$coefficients, fvec = -residual, termcd = code,
           iter = 5L, message = "Mocked solver result")
    },
    .package = "nleqslv"
  )
  fit_verbose <- function() {
    est_pw(list(sc, des1), method = "calibration", p_formula = ~agecat + race,
           control = ctrl, verbose = TRUE)
  }
  cases <- list(
    list(code = 2L, residual = 1e-6, status = "Met", warnings = 0L),
    list(code = 2L, residual = 2e-6, status = "Not met", warnings = 1L),
    list(code = 3L, residual = 1e-6, status = "Met", warnings = 1L)
  )
  for (case in cases) {
    code <- case$code
    residual <- case$residual
    messages <- character()
    warnings <- character()
    fit <- withCallingHandlers(
      fit_verbose(),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, case$warnings)
    expect_identical(fit$solver_diagnostics$fmax, residual)
    expect_identical(fit$solver_diagnostics$ftol, 1e-6)
    expect_equal(fit$pseudo_weights, fit_cali$pseudo_weights)
    status <- paste("Equation tolerance:", case$status)
    expect_true(any(grepl(status, messages, fixed = TRUE)))
    expect_output(print(fit), status, fixed = TRUE)
    expect_output(summary(fit), status, fixed = TRUE)
  }
  code <- 3L
  residual <- 2e-6
  expect_error(suppressMessages(fit_verbose()), "no better point was found")
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
