test_that("confint uses adjusted standard errors and supports level and parm", {
  out <- structure(list(estimates = data.frame(
    domain = c("Group B", "Group A"),
    adjusted_mean = c(3, 4), adjusted_se = c(0.5, 0)
  )), class = "pwmean")

  ci <- confint(out)
  expect_identical(dim(ci), c(2L, 2L))
  expect_identical(rownames(ci), names(coef(out)))
  expect_identical(colnames(ci), c("2.5%", "97.5%"))
  expect_equal(unname(ci[1, ]), 3 + c(-1, 1) * qnorm(0.975) * 0.5)
  expect_equal(unname(ci[2, ]), c(4, 4))

  ci90 <- confint(out, level = 0.90)
  expect_identical(colnames(ci90), c("5%", "95%"))
  expect_equal(unname(ci90[1, ]), 3 + c(-1, 1) * qnorm(0.95) * 0.5)
  expect_identical(confint(out, parm = "Group A"), ci[2, , drop = FALSE])
  expect_identical(confint(out, parm = c(2, 1)), ci[c(2, 1), , drop = FALSE])
  expect_identical(confint(out, parm = c("Group A", "Group B")),
                   ci[c(2, 1), , drop = FALSE])
  expect_identical(dim(confint(out, parm = integer())), c(0L, 2L))
})

test_that("confint computes finite limits at a level just below one", {
  out <- structure(list(estimates = data.frame(
    domain = c("Group A", "Group B"),
    adjusted_mean = c(1, 2), adjusted_se = c(0.1, 0)
  )), class = "pwmean")

  ci <- confint(out, level = 1 - .Machine$double.eps / 2)
  critical <- -qnorm(.Machine$double.eps / 4)
  expect_equal(unname(ci[1, ]), 1 + c(-1, 1) * critical * 0.1)
  expect_identical(unname(ci[2, ]), c(2, 2))
})

test_that("confint validates level and estimate selection", {
  out <- structure(list(estimates = data.frame(
    domain = "Overall", adjusted_mean = 1, adjusted_se = 0.1
  )), class = "pwmean")

  for (level in list(0, 1, -0.1, 1.1, NA_real_, Inf, numeric(), c(0.9, 0.95), "0.95")) {
    expect_error(confint(out, level = level), "`level`", fixed = TRUE)
  }
  for (parm in list("unknown", NA_character_, 0, -1, 1.5, 2, NA_real_, Inf, TRUE)) {
    expect_error(confint(out, parm = parm), "`parm`", fixed = TRUE)
  }
})

test_that("confint inherits factor labels and does not truncate proportions", {
  out <- structure(list(estimates = data.frame(
    category = c("y = yes", "y = no"), domain = c("Overall", "Overall"),
    adjusted_mean = c(0.05, 0.95), adjusted_se = c(0.1, 0.1)
  )), class = c("pwmean_factor", "pwmean"))

  ci <- confint(out)
  expect_identical(rownames(ci), c("y = yes | Overall", "y = no | Overall"))
  expect_lt(ci[1, 1], 0)
  expect_gt(ci[2, 2], 1)
  expect_identical(confint(out, parm = "y = no | Overall"),
                   ci[2, , drop = FALSE])
})

test_that("confint selects each estimate by name when category and domain labels collide", {
  domains <- c("B | domain = C", "C", "C.1")
  outcome <- rep("A", 18)
  outcome[c(6, 11, 12, 17, 18)] <- "A | domain = B"
  sc <- data.frame(
    x = seq_len(18),
    outcome = factor(outcome),
    domain = factor(rep(domains, each = 6), levels = domains)
  )
  ref <- survey::svydesign(ids = ~1, weights = ~wt,
                          data = data.frame(x = sc$x, wt = 10))
  fit <- est_pw(list(sc, ref), p_formula = ~x)
  out <- pwmean(fit, y = "outcome", zcol = "domain")

  values <- coef(out)
  label <- "outcome = A | domain = B | domain = C"
  expect_identical(unname(values), out$estimates$adjusted_mean)
  expect_identical(anyDuplicated(names(values)), 0L)
  expect_identical(names(values)[c(1, 5, 6)], c(label, paste0(label, c(".2", ".1"))))

  ci <- confint(out)
  expect_identical(rownames(ci), names(values))
  positions <- c(5L, 1L, 6L)
  expect_identical(confint(out, parm = names(values)[positions]),
                   ci[positions, , drop = FALSE])
})

test_that("confint matches stored intervals for all estimation methods", {
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
      method = method, control = pw_solver_control(ftol = 1e-6)
    ))
    for (outcome in c("psa_level", "diabetes")) {
      for (domain in list(NULL, "race")) {
        out <- pwmean(fit, y = outcome, zcol = domain)
        ci <- confint(out)
        expect_identical(rownames(ci), names(coef(out)))
        expect_equal(unname(ci), unname(as.matrix(
          out$estimates[, c("adjusted_lower", "adjusted_upper")]
        )))
      }
    }
  }
})
