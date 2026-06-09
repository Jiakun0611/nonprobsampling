# test-build_s5_multi.R
# Tests for multi-reference calibration helper functions and ipwm_multi_build()
#
# Covered functions:
#   multi_raking_block_cols()
#   multi_raking_fp()
#   multi_raking_start()
#   multi_raking_fn()
#   multi_raking_jac()
#   ipwm_multi_build()
#
# Setup:
#   Two reference samples, each contributing different variables, so the
#   number of columns lines up exactly (ncol(Xc) = sum(ncol(Xp_list))):
#     sp1 covers x1, x2  -> Xp1 is 3 columns: intercept, x1, x2
#     sp2 adds x3 only   -> Xp2 is 1 column:  x3 (no intercept)
#     Xc is 4 columns:   intercept, x1, x2, x3
#   So block_cols[[1]] = c(1,2,3), block_cols[[2]] = c(4), and f_p has length 4.

# Setup ----
suppressWarnings(library(survey))

# shared test fixtures ----

set.seed(2026)
n_sc  <- 100L
n_sp1 <- 200L
n_sp2 <- 150L

sc_df  <- data.frame(x1 = rnorm(n_sc),  x2 = rnorm(n_sc),  x3 = rnorm(n_sc))
sp1_df <- data.frame(
  x1      = rnorm(n_sp1),
  x2      = rnorm(n_sp1),
  sp_wts1 = runif(n_sp1, 10, 100)
)
sp2_df <- data.frame(
  x1      = rnorm(n_sp2),
  x2      = rnorm(n_sp2),
  x3      = rnorm(n_sp2),
  sp_wts2 = runif(n_sp2, 10, 100)
)

# xcol[[1]] covers {x1,x2}; xcol[[2]] = {x3} (new variable only)
# Xp1 has intercept (first block); Xp2 does NOT (subsequent block)
vars_XC <- c("x1", "x2", "x3")
Xc  <- add_intercept(vars_XC,        data = sc_df,  intercept = TRUE)   # 150 x 4
Xp1 <- add_intercept(c("x1", "x2"), data = sp1_df, intercept = TRUE)   # 200 x 3
Xp2 <- add_intercept("x3",           data = sp2_df, intercept = FALSE)  # 120 x 1

Xp_list  <- list(Xp1, Xp2)

wts1     <- sp1_df$sp_wts1
wts2     <- sp2_df$sp_wts2
wts_list <- list(wts1, wts2)

sp1_des <- survey::svydesign(ids = ~1, weights = ~sp_wts1, data = sp1_df)
sp2_des <- survey::svydesign(ids = ~1, weights = ~sp_wts2, data = sp2_df)

ctrl <- pw_solver_control(ftol = 1e-4)

# Pre-compute shared derived objects for reuse in later sections
block_cols <- multi_raking_block_cols(Xc, Xp_list)
f_p        <- multi_raking_fp(Xp_list, wts_list)

# Generalized estimating equation: multiple reference samples ----
#
# SIM paper equation (9):
#
#   sum_sc w(x_i, beta) x_i^(pk)
#     - sum_spk d_i^(pk) x_i^(pk) = 0,
#   for k = 1, ..., K.
#
# The factor N^{-1} is omitted because it does not affect whether
# the estimating equation equals zero.
#
# For multi-reference calibration:
#   w(x, beta) = exp(-x beta)
#
# Each reference sample contributes a block of variables. The columns
# in Xc corresponding to each reference block are given by block_cols.

general_ee_multi_ref <- function(beta, Xc, Xp_list, wts_list, block_cols) {
  eta_c <- drop(Xc %*% beta)
  w_c   <- exp(-eta_c)

  ee_list <- vector("list", length(Xp_list))

  for (k in seq_along(Xp_list)) {
    Xc_block <- Xc[, block_cols[[k]], drop = FALSE]
    Xp_block <- Xp_list[[k]]
    wts_k    <- wts_list[[k]]

    ee_list[[k]] <- colSums(w_c * Xc_block) - colSums(wts_k * Xp_block)
  }

  unlist(ee_list, use.names = TRUE)
}

# multi_raking_block_cols ----

test_that("multi_raking_block_cols: returns a list of length equal to Xp_list", {
  expect_type(block_cols, "list")
  expect_length(block_cols, 2L)
})

test_that("multi_raking_block_cols: each element is an integer vector", {
  expect_type(block_cols[[1]], "integer")
  expect_type(block_cols[[2]], "integer")
})

test_that("multi_raking_block_cols: block 1 maps to columns 1-3 of Xc", {
  # Xp1 columns: "(Intercept)", "x1", "x2" — all present in Xc at positions 1,2,3
  expect_equal(block_cols[[1]], c(1L, 2L, 3L))
})

test_that("multi_raking_block_cols: block 2 maps to column 4 of Xc", {
  # Xp2 column: "x3" — present in Xc at position 4
  expect_equal(block_cols[[2]], 4L)
})

test_that("multi_raking_block_cols: column names at mapped positions match Xp colnames", {
  expect_equal(colnames(Xc)[block_cols[[1]]], colnames(Xp1))
  expect_equal(colnames(Xc)[block_cols[[2]]], colnames(Xp2))
})

test_that("multi_raking_block_cols: error when Xc has no colnames", {
  Xc_bad <- Xc
  colnames(Xc_bad) <- NULL
  expect_error(
    multi_raking_block_cols(Xc_bad, Xp_list),
    "Xc must have column names",
    fixed = TRUE
  )
})

test_that("multi_raking_block_cols: error when Xp_list[[j]] has no colnames", {
  Xp_bad    <- Xp_list
  colnames(Xp_bad[[2]]) <- NULL
  expect_error(
    multi_raking_block_cols(Xc, Xp_bad),
    "must have column names",
    fixed = TRUE
  )
})

test_that("multi_raking_block_cols: error when Xp_list[[j]] has a column not in Xc", {
  Xp_bad           <- Xp_list
  colnames(Xp_bad[[2]]) <- "z_unknown"
  expect_error(
    multi_raking_block_cols(Xc, Xp_bad),
    "not found in Xc",
    fixed = TRUE
  )
})


# multi_raking_fp ----

test_that("multi_raking_fp: returns a named numeric vector", {
  expect_type(f_p, "double")
  expect_false(is.null(names(f_p)))
})

test_that("multi_raking_fp: length equals total columns across Xp_list", {
  expected_len <- ncol(Xp1) + ncol(Xp2)   # 3 + 1 = 4
  expect_length(f_p, expected_len)
})

test_that("multi_raking_fp: all values are finite", {
  expect_true(all(is.finite(f_p)))
})

test_that("multi_raking_fp: first block matches manual colSums(wts1 * Xp1)", {
  expected_block1 <- colSums(wts1 * Xp1)
  expect_equal(f_p[seq_len(ncol(Xp1))], expected_block1)
})

test_that("multi_raking_fp: second block matches manual colSums(wts2 * Xp2)", {
  idx <- seq(ncol(Xp1) + 1L, ncol(Xp1) + ncol(Xp2))
  expected_block2 <- colSums(wts2 * Xp2)
  expect_equal(f_p[idx], expected_block2)
})

test_that("multi_raking_fp: error when Xp_list and wts_list have different lengths", {
  expect_error(
    multi_raking_fp(Xp_list, list(wts1)),   # 2 vs 1
    "must have the same length",
    fixed = TRUE
  )
})

test_that("multi_raking_fp: error when nrow(Xp) != length(wts)", {
  wts_bad <- list(wts1, wts2[-1])   # wts2 shortened by 1
  expect_error(
    multi_raking_fp(Xp_list, wts_bad),
    "does not match",
    fixed = TRUE
  )
})


# multi_raking_start ----

beta_start <- multi_raking_start(
  Xc         = Xc,
  Xp_list    = Xp_list,
  block_cols = block_cols,
  f_p        = f_p
)

test_that("multi_raking_start: returns a numeric vector of length ncol(Xc)", {
  expect_type(beta_start, "double")
  expect_length(beta_start, ncol(Xc))
})

test_that("multi_raking_start: all values are finite", {
  expect_true(all(is.finite(beta_start)))
})

test_that("multi_raking_start: non-intercept values are zero", {
  # Only position 1 (the intercept) should be non-zero
  expect_equal(beta_start[-1], rep(0, ncol(Xc) - 1L))
})

test_that("multi_raking_start: intercept initialized to -log(sum(wts1) / n_sc)", {
  # At beta=0: eta=0, wts_sc=1, f_c[1] = sum(Xc[,1]) = n_sc
  # f_p[1] = sum(wts1 * Xp1[,1]) = sum(wts1)
  expected_intercept <- -log(sum(wts1) / n_sc)
  expect_equal(beta_start[1], expected_intercept, tolerance = 1e-10)
})


# multi_raking_fn ----

test_that("multi_raking_fn: returns a numeric vector of length equal to length(f_p)", {
  fn_val <- multi_raking_fn(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  expect_type(fn_val, "double")
  expect_length(fn_val, length(f_p))
})

test_that("multi_raking_fn: all values are finite at beta_start", {
  fn_val <- multi_raking_fn(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  expect_true(all(is.finite(fn_val)))
})

test_that("multi_raking_fn: not zero at beta_start (system not solved)", {
  fn_val <- multi_raking_fn(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  # Non-intercept components should differ from zero
  expect_true(max(abs(fn_val)) > 1e-6)
})

test_that("multi_raking_fn: near zero at converged beta", {
  sol <- solve_participation_model(
    beta_start = beta_start,
    fn         = multi_raking_fn,
    jac        = multi_raking_jac,
    label      = "Test",
    control    = ctrl,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  fn_val <- multi_raking_fn(
    beta       = sol$coefficients,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  expect_true(max(abs(fn_val)) < 1e-4)
})

test_that("multi_raking_fn: error on non-finite beta", {
  beta_bad    <- beta_start
  beta_bad[1] <- Inf
  expect_error(
    multi_raking_fn(
      beta       = beta_bad,
      Xc         = Xc,
      f_p        = f_p,
      block_cols = block_cols
    ),
    "non-finite",
    fixed = TRUE
  )
})


# multi_raking_jac ----

test_that("multi_raking_jac: returns a matrix of dim (length(f_p) x ncol(Xc))", {
  J <- multi_raking_jac(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  expect_true(is.matrix(J))
  expect_equal(dim(J), c(length(f_p), ncol(Xc)))
})

test_that("multi_raking_jac: diagonal values are all negative at beta_start", {
  J <- multi_raking_jac(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )
  # The p x p Jacobian should be negative on the diagonal
  diag_values <- diag(J)
  expect_true(all(diag_values < 0))
})

test_that("multi_raking_jac returns the same Jacobian as numerical differentiation", {
  beta0 <- beta_start
  eps   <- 1e-6
  p     <- ncol(Xc)
  m     <- length(f_p)

  J_num <- matrix(0, nrow = m, ncol = p)
  for (k in seq_len(p)) {
    b_up      <- beta0; b_up[k] <- beta0[k] + eps
    b_dn      <- beta0; b_dn[k] <- beta0[k] - eps
    fn_up <- multi_raking_fn(b_up, Xc = Xc, f_p = f_p, block_cols = block_cols)
    fn_dn <- multi_raking_fn(b_dn, Xc = Xc, f_p = f_p, block_cols = block_cols)
    J_num[, k] <- (fn_up - fn_dn) / (2 * eps)
  }

  J_ana <- multi_raking_jac(
    beta       = beta0,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )

  # unname() removes dimnames inherited by J_ana from matrix multiplication
  expect_equal(unname(J_ana), J_num, tolerance = 1e-4)
})

test_that("multi_raking_jac: error on non-finite beta", {
  beta_bad    <- beta_start
  beta_bad[2] <- NaN
  expect_error(
    multi_raking_jac(
      beta       = beta_bad,
      Xc         = Xc,
      f_p        = f_p,
      block_cols = block_cols
    ),
    "non-finite",
    fixed = TRUE
  )
})


# ipwm_multi_build ----

# For ipwm_multi_build the sp list and vars list must align with the
# design objects.  We pass the FULL variable sets per sample; the
# builder internally derives xcol (new-variables-per-sample) via
# check_input_multi.

sp_list    <- list(sp1_df, sp2_df)
vars_list  <- list(c("x1", "x2"), c("x1", "x2", "x3"))
weight_vec <- c("sp_wts1", "sp_wts2")
des_list   <- list(sp1_des, sp2_des)

build_multi <- function(...) {
  ipwm_multi_build(
    sc       = sc_df,
    sp       = sp_list,
    vars     = vars_list,
    weight   = weight_vec,
    sp_des   = des_list,
    sp_order = "given",
    control  = ctrl,
    ...
  )
}

out_multi <- build_multi()

# return structure ----

test_that("ipwm_multi_build: returns a list with required top-level names", {
  expected <- c("pseudo_weights", "coefficients", "method",
                "solver_diagnostics", "internal")
  expect_true(all(expected %in% names(out_multi)))
})

test_that("ipwm_multi_build: method is 'multi'", {
  expect_equal(out_multi$method, "multi")
})

# pseudo-weights ----

test_that("ipwm_multi_build: pseudo_weights has length n_sc", {
  expect_length(out_multi$pseudo_weights, n_sc)
})

test_that("ipwm_multi_build: all pseudo-weights are positive", {
  expect_true(all(out_multi$pseudo_weights > 0))
})

test_that("ipwm_multi_build: all pseudo-weights are finite", {
  expect_true(all(is.finite(out_multi$pseudo_weights)))
})

# coefficients ----

test_that("ipwm_multi_build: coefficients has length ncol(Xc) = 4", {
  expect_length(out_multi$coefficients, 4L)
})

test_that("ipwm_multi_build: coefficients are named", {
  expect_false(is.null(names(out_multi$coefficients)))
  expect_true("(Intercept)" %in% names(out_multi$coefficients))
})

test_that("ipwm_multi_build: all coefficients are finite", {
  expect_true(all(is.finite(out_multi$coefficients)))
})


# estimation equation (9) in SIM satisfied ---
test_that("ipwm_multi_build satisfies the multi-reference estimating equation", {
  int_obj <- out_multi$internal

  ee <- general_ee_multi_ref(
    beta       = out_multi$coefficients,
    Xc         = int_obj$Xc,
    Xp_list    = int_obj$Xp_list,
    wts_list   = int_obj$wts_list,
    block_cols = int_obj$block_cols
  )

  expect_lt(max(abs(ee)), 1e-4)
})


test_that("multi_raking_fn matches the multi-reference estimating equation helper", {
  ee_manual <- general_ee_multi_ref(
    beta       = beta_start,
    Xc         = Xc,
    Xp_list    = Xp_list,
    wts_list   = wts_list,
    block_cols = block_cols
  )

  ee_function <- multi_raking_fn(
    beta       = beta_start,
    Xc         = Xc,
    f_p        = f_p,
    block_cols = block_cols
  )

  expect_equal(unname(ee_function), unname(ee_manual), tolerance = 1e-10)
})




# solver_diagnostics ----

test_that("ipwm_multi_build: solver_diagnostics has expected names", {
  expected <- c("solver", "termcd", "message", "method", "iter", "fmax")
  expect_true(all(expected %in% names(out_multi$solver_diagnostics)))
})

test_that("ipwm_multi_build: solver converged (termcd 1 or 2)", {
  expect_true(out_multi$solver_diagnostics$termcd %in% c(1L, 2L))
})

test_that("ipwm_multi_build: fmax is small after convergence", {
  expect_true(out_multi$solver_diagnostics$fmax < 1e-4)
})

# internal ----

test_that("ipwm_multi_build: internal contains expected components", {
  expected <- c("Xc", "Xp_list", "wts_list", "f_p",
                "block_cols", "D", "S_beta")
  expect_true(all(expected %in% names(out_multi$internal)))
})

test_that("ipwm_multi_build: internal$Xc has n_sc rows and 4 columns", {
  expect_equal(nrow(out_multi$internal$Xc), n_sc)
  expect_equal(ncol(out_multi$internal$Xc), 4L)
})

test_that("ipwm_multi_build: internal$Xp_list has length 2", {
  expect_length(out_multi$internal$Xp_list, 2L)
})

test_that("ipwm_multi_build: internal$f_p has length 4", {
  expect_length(out_multi$internal$f_p, 4L)
})

test_that("ipwm_multi_build: internal$block_cols has length 2", {
  expect_length(out_multi$internal$block_cols, 2L)
})

test_that("ipwm_multi_build: internal$S_beta is a 4x4 matrix", {
  expect_true(is.matrix(out_multi$internal$S_beta))
  expect_equal(dim(out_multi$internal$S_beta), c(4L, 4L))
})

test_that("ipwm_multi_build: internal$D is a square matrix of dim 4", {
  # D is block-diagonal: 3x3 for block 1 + 1x1 for block 2
  expect_true(is.matrix(out_multi$internal$D))
  expect_equal(dim(out_multi$internal$D), c(4L, 4L))
})

# sp_order options ----

test_that("ipwm_multi_build: sp_order = 'size' runs without error", {
  expect_no_error(
    ipwm_multi_build(
      sc       = sc_df,
      sp       = sp_list,
      vars     = vars_list,
      weight   = weight_vec,
      sp_des   = des_list,
      sp_order = "size",
      control  = ctrl
    )
  )
})

test_that("ipwm_multi_build: sp_order = 'given' and 'size' produce positive pseudo-weights", {
  out_size  <- ipwm_multi_build(
    sc       = sc_df,
    sp       = sp_list,
    vars     = vars_list,
    weight   = weight_vec,
    sp_des   = des_list,
    sp_order = "size",
    control  = ctrl
  )
  expect_true(all(out_size$pseudo_weights > 0))
})

# log_messages forwarded ----

test_that("ipwm_multi_build: log_messages argument is passed to internal", {
  out_log <- ipwm_multi_build(
    sc           = sc_df,
    sp           = sp_list,
    vars         = vars_list,
    weight       = weight_vec,
    sp_des       = des_list,
    sp_order     = "given",
    control      = ctrl,
    log_messages = c("upstream_msg")
  )
  expect_true("upstream_msg" %in% out_log$internal$log_messages)
})

# verbose does not error ----

test_that("ipwm_multi_build: verbose = TRUE runs without error", {
  expect_no_error(
    build_multi(verbose = TRUE)
  )
})
