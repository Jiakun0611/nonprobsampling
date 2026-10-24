test_that("coef.pw_fit extracts participation model coefficients for all methods", {
  data(sc)
  data(sp1)
  data(sp2)

  des1 <- survey::svydesign(
    ids = ~psu_sp1, strata = ~strata_sp1, weights = ~wts_sp1,
    data = sp1, nest = TRUE
  )
  des2 <- survey::svydesign(
    ids = ~psu_sp2, strata = ~strata_sp2, weights = ~wts_sp2,
    data = sp2, nest = TRUE
  )

  for (method in c("calibration", "alp", "clw", "multi")) {
    multi <- identical(method, "multi")
    fit <- suppressMessages(est_pw(
      data = if (multi) list(sc, des1, des2) else list(sc, des1),
      p_formula = if (multi) list(~agecat, ~race) else ~agecat + race,
      method = method,
      control = pw_solver_control(ftol = 1e-6)
    ))

    expect_type(stats::coef(fit), "double")
    expect_false(is.null(names(stats::coef(fit))))
    expect_identical(stats::coef(fit), fit$coefficients)
    expect_identical(stats::coefficients(fit), fit$coefficients)
  }
})

test_that("coef.pw_fit returns NULL when coefficients are absent", {
  fit <- structure(list(), class = "pw_fit")
  expect_null(stats::coef(fit))
})
