# test-build_s5_one.R
# Tests for Step 5 one-reference builders:
#   raking_fn / raking_jac / raking_start / raking_build
#   alp_fn    / alp_jac    / alp_start    / alp_build
#   clw_fn    / clw_jac    / clw_start    / clw_build
#   ipwm_one_build  (dispatcher)

# Shared test data ----
# Plain numeric covariates: no factor expansion.
# n_sc = 200, n_sp = 300, p = 3 (intercept + x1 + x2).

set.seed(42)

n_sc <- 200L
n_sp <- 300L

sc_df <- data.frame(
  x1 = rnorm(n_sc),
  x2 = rnorm(n_sc)
)

sp_df <- data.frame(
  x1     = rnorm(n_sp),
  x2     = rnorm(n_sp),
  sp_wts = runif(n_sp, 10, 100)
)

vars   <- c("x1", "x2")
Xc     <- add_intercept(vars, sc_df)      # 200 x 3
Xp     <- add_intercept(vars, sp_df)      # 300 x 3
wts_sp <- sp_df$sp_wts
f_p    <- colSums(wts_sp * Xp)

sp_des <- survey::svydesign(ids = ~1, weights = ~sp_wts, data = sp_df)
ctrl   <- pw_solver_control()




# Generalized estimating equation: one reference sample ----
#
# SIM paper equation (2):
#
#   sum_sc w(x_i, beta) h(x_i, beta)
#     - sum_sp d_j h(x_j, beta) = 0
#
#
# Method-specific choices:
#
# Calibration:
#   w(x, beta) = exp(-x beta)
#   h(x, beta) = x
#
# ALP:
#   w(x, beta) = exp(-x beta)
#   h(x, beta) = [1 + w(x, beta)]^{-1} x
#              = expit(x beta) x
#
#   This gives:
#   sum_sc (1 - p_i) x_i - sum_sp d_j p_j x_j = 0
#
# CLW:
#   w(x, beta) = 1 + exp(-x beta)
#   h(x, beta) = [w(x, beta)]^{-1} x
#
#   This gives:
#   sum_sc x_i - sum_sp d_j expit(x_j beta) x_j = 0

general_ee_one_ref <- function(beta, Xc, Xp, wts_sp, method) {
  eta_c <- drop(Xc %*% beta)
  eta_p <- drop(Xp %*% beta)

  if (method == "calibration") {
    w_c <- exp(-eta_c)

    ee <- colSums(w_c * Xc) - colSums(wts_sp * Xp)

  } else if (method == "alp") {
    p_c <- expit(eta_c)
    p_p <- expit(eta_p)

    w_c <- exp(-eta_c)

    ee <- colSums(w_c * p_c * Xc) - colSums(wts_sp * p_p * Xp)

  } else if (method == "clw") {
    pi_p <- expit(eta_p)

    ee <- colSums(Xc) - colSums(wts_sp * pi_p * Xp)

  } else {
    stop("Unknown method.", call. = FALSE)
  }

  ee
}

# raking_fn ----

test_that("raking_fn: returns a finite numeric vector of length p at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  out   <- raking_fn(beta0, Xc = Xc, f_p = f_p)

  expect_type(out, "double")
  expect_length(out, ncol(Xc))
  expect_true(all(is.finite(out)))
})

test_that("raking_fn: non-intercept values differ from zero at beta = 0", {
  # At beta = 0, weights = 1 for all rows; f_c = colSums(Xc) != f_p in general
  beta0 <- rep(0, ncol(Xc))
  out   <- raking_fn(beta0, Xc = Xc, f_p = f_p)

  expect_true(any(out != 0))
})


# raking_jac ----

test_that("raking_jac: returns a square finite matrix at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  J     <- raking_jac(beta0, Xc = Xc, f_p = f_p)

  expect_true(is.matrix(J))
  expect_equal(nrow(J), ncol(Xc))
  expect_equal(ncol(J), ncol(Xc))
  expect_true(all(is.finite(J)))
})

test_that("raking_jac: diagonal values are all negative at beta = 0", {
  # J = -t(wts_sc * Xc) %*% Xc; diagonal is -colSums(wts_sc * Xc^2) < 0
  beta0 <- rep(0, ncol(Xc))
  J     <- raking_jac(beta0, Xc = Xc, f_p = f_p)

  expect_true(all(diag(J) < 0))
})


# raking_start ----

test_that("raking_start: returns a finite numeric vector of length p", {
  bs <- raking_start(Xc = Xc, f_p = f_p)

  expect_type(bs, "double")
  expect_length(bs, ncol(Xc))
  expect_true(all(is.finite(bs)))
})

test_that("raking_start: intercept initialized to -log(sum_wts / n_sc)", {
  # At beta = 0: wts0 = rep(1, n_sc), f_c0[1] = n_sc, f_p[1] = sum(wts_sp)
  bs       <- raking_start(Xc = Xc, f_p = f_p)
  expected <- -log(sum(wts_sp) / n_sc)

  expect_equal(bs[1], expected, tolerance = 1e-10)
})

test_that("raking_start: non-intercept values are zero", {
  bs <- raking_start(Xc = Xc, f_p = f_p)

  expect_equal(bs[-1], rep(0, ncol(Xc) - 1))
})


# raking_build ----

test_that("raking_build: return list has expected names", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_named(out, c("weights", "coefficients", "solver_diagnostics",
                      "log_messages", "internal"))
})

test_that("raking_build: pseudo-weights are positive and finite", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_true(all(is.finite(out$weights)))
  expect_true(all(out$weights > 0))
})

test_that("raking_build: pseudo-weights have length nrow(sc)", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_length(out$weights, n_sc)
})

test_that("raking_build: calibration equations satisfied (max|eq| < 1e-6)", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)
  res <- raking_fn(out$coefficients, Xc = Xc, f_p = f_p)

  expect_lt(max(abs(res)), 1e-6)
})

test_that("raking_build: coefficients are named by Xc column names", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_named(out$coefficients, colnames(Xc))
})

test_that("raking_build: solver converged (termcd in 1 or 2)", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_true(out$solver_diagnostics$termcd %in% c(1L, 2L))
})

test_that("raking_build: internal contains Xc, Xp, S_beta, D", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)

  expect_true(all(c("Xc", "Xp", "S_beta", "D") %in% names(out$internal)))
})

test_that("raking_build: S_beta is a square p x p matrix", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)
  p   <- ncol(Xc)

  expect_equal(dim(out$internal$S_beta), c(p, p))
})

test_that("raking_build: D is a square matrix", {
  out <- raking_build(vars, sc_df, sp_df, sp_des,
                      wts.col = "sp_wts", control = ctrl)
  D   <- out$internal$D

  expect_true(is.matrix(D))
  expect_equal(nrow(D), ncol(D))
})

test_that("raking_build: errors when weight column not in sp", {
  expect_error(
    raking_build(vars, sc_df, sp_df, sp_des,
                 wts.col = "not_exist", control = ctrl),
    "weight column 'not_exist' was not found",
    fixed = TRUE
  )
})

test_that("raking_build: errors when sp weights contain a negative value", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- -1

  expect_error(
    raking_build(vars, sc_df, sp_bad, sp_des,
                 wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})

test_that("raking_build: errors when sp weights contain NA", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- NA_real_

  expect_error(
    raking_build(vars, sc_df, sp_bad, sp_des,
                 wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})


# alp_fn ----

test_that("alp_fn: returns a finite numeric vector of length p at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  out   <- alp_fn(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_type(out, "double")
  expect_length(out, ncol(Xc))
  expect_true(all(is.finite(out)))
})

test_that("alp_fn: non-intercept values differ from zero at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  out   <- alp_fn(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(any(out != 0))
})


# alp_jac ----

test_that("alp_jac: returns a square finite matrix at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  J     <- alp_jac(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(is.matrix(J))
  expect_equal(nrow(J), ncol(Xc))
  expect_equal(ncol(J), ncol(Xc))
  expect_true(all(is.finite(J)))
})

test_that("alp_jac: diagonal values are all negative at beta = 0", {
  # J = -t(p*(1-p)*Xc)%*%Xc - t(wts*p*(1-p)*Xp)%*%Xp; both terms are negative
  beta0 <- rep(0, ncol(Xc))
  J     <- alp_jac(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(all(diag(J) < 0))
})


# alp_start ----

test_that("alp_start: returns a finite numeric vector of length p", {
  bs <- alp_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_type(bs, "double")
  expect_length(bs, ncol(Xc))
  expect_true(all(is.finite(bs)))
})

test_that("alp_start: intercept initialized to -log(sum_wts / n_sc)", {
  # At beta = 0: expit(0) = 0.5 everywhere.
  # f_p0[1] = 0.5 * sum(wts_sp), f_c0[1] = 0.5 * n_sc -> ratio = sum(wts_sp)/n_sc
  bs       <- alp_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)
  expected <- -log(sum(wts_sp) / n_sc)

  expect_equal(bs[1], expected, tolerance = 1e-10)
})

test_that("alp_start: non-intercept values are zero", {
  bs <- alp_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_equal(bs[-1], rep(0, ncol(Xc) - 1))
})


# alp_build ----

test_that("alp_build: return list has expected names", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_named(out, c("weights", "coefficients", "solver_diagnostics",
                      "log_messages", "internal"))
})

test_that("alp_build: pseudo-weights are positive and finite", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(is.finite(out$weights)))
  expect_true(all(out$weights > 0))
})

test_that("alp_build: pseudo-weights have length nrow(sc)", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_length(out$weights, n_sc)
})

test_that("alp_build: calibration equations satisfied (max|eq| < 1e-6)", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)
  res <- alp_fn(out$coefficients, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_lt(max(abs(res)), 1e-6)
})

test_that("alp_build: coefficients are named by Xc column names", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_named(out$coefficients, colnames(Xc))
})

test_that("alp_build: solver converged (termcd in 1 or 2)", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(out$solver_diagnostics$termcd %in% c(1L, 2L))
})

test_that("alp_build: internal contains Xc, Xp, p_sc, p_sp, S_beta, D", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(c("Xc", "Xp", "p_sc", "p_sp", "S_beta", "D") %in%
                    names(out$internal)))
})

test_that("alp_build: estimated probabilities p_sc are strictly in (0, 1)", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(out$internal$p_sc > 0 & out$internal$p_sc < 1))
})

test_that("alp_build: estimated probabilities p_sp are strictly in (0, 1)", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(out$internal$p_sp > 0 & out$internal$p_sp < 1))
})

test_that("alp_build: S_beta is a square p x p matrix", {
  out <- alp_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)
  p   <- ncol(Xc)

  expect_equal(dim(out$internal$S_beta), c(p, p))
})

test_that("alp_build: errors when weight column not in sp", {
  expect_error(
    alp_build(vars, sc_df, sp_df, sp_des,
              wts.col = "not_exist", control = ctrl),
    "weight column 'not_exist' was not found",
    fixed = TRUE
  )
})

test_that("alp_build: errors when sp weights contain a negative value", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- -1

  expect_error(
    alp_build(vars, sc_df, sp_bad, sp_des,
              wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})

test_that("alp_build: errors when sp weights contain NA", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- NA_real_

  expect_error(
    alp_build(vars, sc_df, sp_bad, sp_des,
              wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})


# clw_fn ----

test_that("clw_fn: returns a finite numeric vector of length p at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  out   <- clw_fn(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_type(out, "double")
  expect_length(out, ncol(Xc))
  expect_true(all(is.finite(out)))
})

test_that("clw_fn: non-intercept values differ from zero at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  out   <- clw_fn(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(any(out != 0))
})


# clw_jac ----

test_that("clw_jac: returns a square finite matrix at beta = 0", {
  beta0 <- rep(0, ncol(Xc))
  J     <- clw_jac(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(is.matrix(J))
  expect_equal(nrow(J), ncol(Xc))
  expect_equal(ncol(J), ncol(Xc))
  expect_true(all(is.finite(J)))
})

test_that("clw_jac: diagonal values are all negative at beta = 0", {
  # J = -t(wts_sp * pi*(1-pi) * Xp) %*% Xp; diagonal < 0
  beta0 <- rep(0, ncol(Xc))
  J     <- clw_jac(beta0, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_true(all(diag(J) < 0))
})


# clw_start ----

test_that("clw_start: returns a finite numeric vector of length p", {
  bs <- clw_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_type(bs, "double")
  expect_length(bs, ncol(Xc))
  expect_true(all(is.finite(bs)))
})

test_that("clw_start: intercept initialized to -log(0.5 * sum_wts / n_sc)", {
  # At beta = 0: f_p0[1] = 0.5 * sum(wts_sp), f_c0[1] = colSums(Xc)[1] = n_sc
  bs       <- clw_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)
  expected <- -log(0.5 * sum(wts_sp) / n_sc)

  expect_equal(bs[1], expected, tolerance = 1e-10)
})

test_that("clw_start: non-intercept values are zero", {
  bs <- clw_start(Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_equal(bs[-1], rep(0, ncol(Xc) - 1))
})


# clw_build ----

test_that("clw_build: return list has expected names", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_named(out, c("weights", "coefficients", "solver_diagnostics",
                      "log_messages", "internal"))
})

test_that("clw_build: pseudo-weights are positive and finite", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(is.finite(out$weights)))
  expect_true(all(out$weights > 0))
})

test_that("clw_build: pseudo-weights have length nrow(sc)", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_length(out$weights, n_sc)
})

test_that("clw_build: calibration equations satisfied (max|eq| < 1e-6)", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)
  res <- clw_fn(out$coefficients, Xc = Xc, Xp = Xp, wts_sp = wts_sp)

  expect_lt(max(abs(res)), 1e-6)
})

test_that("clw_build: coefficients are named by Xc column names", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_named(out$coefficients, colnames(Xc))
})

test_that("clw_build: solver converged (termcd in 1 or 2)", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(out$solver_diagnostics$termcd %in% c(1L, 2L))
})

test_that("clw_build: internal contains Xc, Xp, pi_sc, pi_sp, S_beta, D", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(c("Xc", "Xp", "pi_sc", "pi_sp", "S_beta", "D") %in%
                    names(out$internal)))
})

test_that("clw_build: estimated probabilities pi_sc are strictly in (0, 1)", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(out$internal$pi_sc > 0 & out$internal$pi_sc < 1))
})

test_that("clw_build: estimated probabilities pi_sp are strictly in (0, 1)", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)

  expect_true(all(out$internal$pi_sp > 0 & out$internal$pi_sp < 1))
})

test_that("clw_build: S_beta is a square p x p matrix", {
  out <- clw_build(vars, sc_df, sp_df, sp_des,
                   wts.col = "sp_wts", control = ctrl)
  p   <- ncol(Xc)

  expect_equal(dim(out$internal$S_beta), c(p, p))
})

test_that("clw_build: errors when weight column not in sp", {
  expect_error(
    clw_build(vars, sc_df, sp_df, sp_des,
              wts.col = "not_exist", control = ctrl),
    "weight column 'not_exist' was not found",
    fixed = TRUE
  )
})

test_that("clw_build: errors when sp weights contain a negative value", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- -1

  expect_error(
    clw_build(vars, sc_df, sp_bad, sp_des,
              wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})

test_that("clw_build: errors when sp weights contain NA", {
  sp_bad           <- sp_df
  sp_bad$sp_wts[1] <- NA_real_

  expect_error(
    clw_build(vars, sc_df, sp_bad, sp_des,
              wts.col = "sp_wts", control = ctrl),
    "reference survey weights must be positive finite numbers",
    fixed = TRUE
  )
})


test_that("one-reference builders satisfy the generalized estimating equation", {
  out_raking <- raking_build(
    vars, sc_df, sp_df, sp_des,
    wts.col = "sp_wts",
    control = ctrl
  )

  ee_raking <- general_ee_one_ref(
    beta = out_raking$coefficients,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp,
    method = "calibration"
  )

  expect_lt(max(abs(ee_raking)), 1e-6)


  out_alp <- alp_build(
    vars, sc_df, sp_df, sp_des,
    wts.col = "sp_wts",
    control = ctrl
  )

  ee_alp <- general_ee_one_ref(
    beta = out_alp$coefficients,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp,
    method = "alp"
  )

  expect_lt(max(abs(ee_alp)), 1e-6)


  out_clw <- clw_build(
    vars, sc_df, sp_df, sp_des,
    wts.col = "sp_wts",
    control = ctrl
  )

  ee_clw <- general_ee_one_ref(
    beta = out_clw$coefficients,
    Xc = Xc,
    Xp = Xp,
    wts_sp = wts_sp,
    method = "clw"
  )

  expect_lt(max(abs(ee_clw)), 1e-6)
})












# ipwm_one_build ----

test_that("ipwm_one_build: return list has expected names", {
  out <- ipwm_one_build(
    sc = sc_df, sp = sp_df, sp_des = sp_des,
    vars = vars, weight = "sp_wts",
    method = "calibration", control = ctrl
  )

  expect_named(out, c("pseudo_weights", "coefficients", "method",
                      "solver_diagnostics", "internal"))
})

test_that("ipwm_one_build (calibration): pseudo_weights are positive with correct length", {
  out <- ipwm_one_build(
    sc = sc_df, sp = sp_df, sp_des = sp_des,
    vars = vars, weight = "sp_wts",
    method = "calibration", control = ctrl
  )

  expect_length(out$pseudo_weights, n_sc)
  expect_true(all(out$pseudo_weights > 0))
  expect_equal(out$method, "calibration")
})

test_that("ipwm_one_build (alp): pseudo_weights are positive with correct length", {
  out <- ipwm_one_build(
    sc = sc_df, sp = sp_df, sp_des = sp_des,
    vars = vars, weight = "sp_wts",
    method = "alp", control = ctrl
  )

  expect_length(out$pseudo_weights, n_sc)
  expect_true(all(out$pseudo_weights > 0))
  expect_equal(out$method, "alp")
})

test_that("ipwm_one_build (clw): pseudo_weights are positive with correct length", {
  out <- ipwm_one_build(
    sc = sc_df, sp = sp_df, sp_des = sp_des,
    vars = vars, weight = "sp_wts",
    method = "clw", control = ctrl
  )

  expect_length(out$pseudo_weights, n_sc)
  expect_true(all(out$pseudo_weights > 0))
  expect_equal(out$method, "clw")
})

test_that("ipwm_one_build: log_messages is added inside internal", {
  out <- ipwm_one_build(
    sc = sc_df, sp = sp_df, sp_des = sp_des,
    vars = vars, weight = "sp_wts",
    method = "calibration", control = ctrl
  )

  expect_true("log_messages" %in% names(out$internal))
})

test_that("ipwm_one_build: errors on unknown method", {
  expect_error(
    ipwm_one_build(
      sc = sc_df, sp = sp_df, sp_des = sp_des,
      vars = vars, weight = "sp_wts",
      method = "bad_method", control = ctrl
    ),
    "Unknown method",
    fixed = TRUE
  )
})
