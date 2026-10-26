make_d_collision_design <- function(collisions = FALSE, replicate = FALSE) {
  dat <- data.frame(x = seq(-2, 2, length.out = 100), wt = 10)
  if (collisions) {
    dat$h_x <- seq_len(nrow(dat))^2
    dat$h_x.1 <- -seq_len(nrow(dat))^3
    dat$h_X.Intercept. <- seq_len(nrow(dat))
    dat$h_X.Intercept..1 <- seq_len(nrow(dat))^2
  }
  des <- survey::svydesign(ids = ~1, weights = ~wt, data = dat)
  if (replicate) des <- survey::as.svrepdesign(des, type = "JK1")
  des
}

test_that("D uses the intended total variables despite existing helper names", {
  for (replicate in c(FALSE, TRUE)) {
    clean <- make_d_collision_design(replicate = replicate)
    collision <- make_d_collision_design(TRUE, replicate = replicate)
    original <- collision
    Xp <- stats::model.matrix(~x, clean$variables)
    probabilities <- seq(0.1, 0.4, length.out = nrow(Xp))

    for (method in c("calibration", "alp", "clw")) {
      multiplier <- if (method == "calibration") rep(1, nrow(Xp)) else {
        probabilities
      }
      # Independent survey-total calculation, with no internal helper names.
      oracle <- clean
      oracle$variables <- data.frame(
        intercept = multiplier, value = multiplier * clean$variables$x
      )
      expected <- stats::vcov(survey::svytotal(~intercept + value - 1, oracle))
      dimnames(expected) <- rep(list(c("h_X.Intercept.", "h_x")), 2)

      compute <- switch(method,
        calibration = function(des) compute_D_raking(des, Xp),
        alp = function(des) compute_D_ALP(des, probabilities, Xp),
        clw = function(des) compute_D_CLW(des, probabilities, Xp)
      )
      expect_equal(compute(clean), expected)
      expect_equal(compute(collision), expected)
      expect_identical(collision, original)
    }
  }
})

test_that("unrelated colliding columns leave fitted variance estimates unchanged", {
  sc <- data.frame(
    x = seq(-1, 3, length.out = 100), y = sin(seq_len(100))
  )
  clean <- make_d_collision_design()
  collision <- make_d_collision_design(TRUE)

  for (method in c("calibration", "alp", "clw")) {
    baseline <- est_pw(list(sc, clean), p_formula = ~x, method = method)
    actual <- est_pw(list(sc, collision), p_formula = ~x, method = method)
    expect_equal(actual$pseudo_weights, baseline$pseudo_weights)
    expect_equal(actual$internal$D, baseline$internal$D)
    expect_equal(pwmean(actual, y = "y")$estimates,
                 pwmean(baseline, y = "y")$estimates)
  }
})

test_that("multi-reference D blocks ignore unrelated colliding columns", {
  clean <- make_d_collision_design()
  collision <- make_d_collision_design(TRUE)
  Xp <- stats::model.matrix(~x, clean$variables)
  expected <- make_block_D_multi(list(clean, clean), list(Xp, Xp))
  actual <- make_block_D_multi(list(collision, collision), list(Xp, Xp))
  expect_equal(actual, expected)
})
