test_that("coef.pwmean returns weighted means in domain row order", {
  out <- structure(list(estimates = data.frame(
    domain = c("Group B", "Group A"),
    unweighted_mean = c(1, 2), adjusted_mean = c(3, 4)
  )), class = "pwmean")

  expect_identical(stats::coef(out), c("Group B" = 3, "Group A" = 4))
})

test_that("coef.pwmean_factor identifies each category and domain", {
  out <- structure(list(estimates = data.frame(
    category = c("y = yes", "y = no", "y = yes", "y = no"),
    domain = c("Group B", "Group B", "Group A", "Group A"),
    adjusted_mean = c(0.3, 0.7, 0.4, 0.6)
  )), class = c("pwmean_factor", "pwmean"))

  expect_identical(stats::coef(out), c(
    "y = yes | Group B" = 0.3, "y = no | Group B" = 0.7,
    "y = yes | Group A" = 0.4, "y = no | Group A" = 0.6
  ))
})

test_that("coef extracts means and proportions for all estimation methods", {
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

    for (outcome in c("psa_level", "diabetes")) {
      for (domain in list(NULL, "race", "diabetes")) {
        out <- pwmean(fit, y = outcome, zcol = domain)
        values <- stats::coef(out)
        expect_type(values, "double")
        expect_identical(unname(values), out$estimates$adjusted_mean)
        if (outcome == "psa_level") {
          expected_names <- out$estimates$domain
        } else {
          expect_s3_class(out, "pwmean_factor")
          expected_names <- paste(out$estimates$category,
                                  out$estimates$domain, sep = " | ")
        }
        expect_identical(names(values), expected_names)
        expect_identical(stats::coefficients(out), values)
      }
    }

    fit$internal$raw_sc$psa_level[1:5] <- NA_real_
    for (na_method in list(stats::na.omit, stats::na.exclude)) {
      out <- pwmean(fit, y = "psa_level", na.action = na_method)
      expect_identical(stats::coef(out),
                       stats::setNames(out$estimates$adjusted_mean, "Overall"))
    }
  }
})
