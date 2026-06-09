# test-estimate.R

# helpers ----

make_est_inputs <- function() {
  n      <- 4L
  Y      <- c(1, 2, 4, 7)
  Z      <- rep(1L, n)
  w      <- rep(2.0, n)
  X      <- cbind("(Intercept)" = 1, x1 = seq_len(n))

  S_beta <- t(w * X) %*% X
  D      <- matrix(0.0, nrow = 2L, ncol = 2L)

  list(
    Y = Y,
    Z = Z,
    w = w,
    X = X,
    D = D,
    S_beta = S_beta
  )
}

make_est_inputs_nonzero_D <- function() {
  inp <- make_est_inputs()

  inp$D <- matrix(
    c(0.20, 0.05,
      0.05, 0.30),
    nrow = 2L,
    byrow = TRUE
  )

  inp
}

make_dispatch_build <- function(method = "alp", inp = make_est_inputs()) {
  structure(
    list(
      method = method,
      internal = list(
        D = inp$D,
        S_beta = inp$S_beta
      )
    ),
    class = "pw_fit"
  )
}

domain_overall <- list(
  mode       = "overall",
  z_name     = NULL,
  labels     = "Overall",
  indicators = NULL
)

domain_binary <- list(
  mode       = "binary",
  z_name     = "grp",
  labels     = "grp = 1",
  indicators = data.frame(.z_domain = c(1L, 1L, 0L, 0L))
)

domain_factor <- list(
  mode       = "factor",
  z_name     = "grp",
  labels     = c("A", "B"),
  indicators = data.frame(
    .z_A = c(1L, 0L, 1L, 0L),
    .z_B = c(0L, 1L, 0L, 1L)
  )
)

make_yz_data <- function(domain_obj) {
  inp <- make_est_inputs()

  list(
    Y = inp$Y,
    w = inp$w,
    X = inp$X,
    domain = domain_obj
  )
}

estimate_funs <- list(
  alp    = alp_estimate,
  clw    = clw_estimate,
  raking = raking_estimate,
  multi  = multi_estimate
)


# output structure ----

test_that("all estimate functions return mean and variance", {
  inp <- make_est_inputs()

  for (fn in estimate_funs) {
    res <- fn(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)

    expect_type(res, "list")
    expect_named(res, c("mean", "variance"))

    expect_length(res$mean, 1L)
    expect_length(res$variance, 1L)

    expect_true(is.finite(res$mean))
    expect_true(is.finite(res$variance))
    expect_gte(res$variance, 0)
  }
})


# mean correctness ----

test_that("all estimate functions compute the pseudo-weighted Hajek mean", {
  inp <- make_est_inputs()

  expected <- sum(inp$Y * inp$Z * inp$w) / sum(inp$Z * inp$w)

  for (fn in estimate_funs) {
    res <- fn(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)
    expect_equal(res$mean, expected, tolerance = 1e-12)
  }
})

test_that("all estimate functions compute domain-specific pseudo-weighted mean", {
  inp   <- make_est_inputs()
  Z_dom <- c(1L, 1L, 0L, 0L)

  expected <- sum(inp$Y * Z_dom * inp$w) / sum(Z_dom * inp$w)

  for (fn in estimate_funs) {
    res <- fn(inp$Y, Z_dom, inp$w, inp$X, inp$D, inp$S_beta)
    expect_equal(res$mean, expected, tolerance = 1e-12)
  }
})


# variance correctness when D = 0 ----

test_that("raking_estimate variance matches analytical value when D = 0", {
  inp <- make_est_inputs()

  res <- raking_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  expect_equal(res$variance, 2 / 64, tolerance = 1e-12)
})

test_that("multi_estimate variance matches raking in one-block D = 0 case", {
  inp <- make_est_inputs()

  res_multi <- multi_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  res_raking <- raking_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  expect_equal(res_multi$variance, res_raking$variance, tolerance = 1e-12)
  expect_equal(res_multi$variance, 2 / 64, tolerance = 1e-12)
})

test_that("alp_estimate variance matches analytical value when D = 0", {
  inp <- make_est_inputs()

  res <- alp_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  expect_equal(res$variance, 89 / 288, tolerance = 1e-12)
})

test_that("clw_estimate variance matches analytical value when D = 0", {
  inp <- make_est_inputs()

  res <- clw_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  expect_equal(res$variance, 49 / 128, tolerance = 1e-12)
})


# variance correctness when D is nonzero ----
# For this synthetic example:
# b_raking = b_alp = c(-5, 2)
# b_clw    = c(-2.5, 1)
# with D = matrix(c(.20, .05, .05, .30), 2, 2, byrow = TRUE)

test_that("raking_estimate variance includes nonzero D component", {
  inp <- make_est_inputs_nonzero_D()

  res <- raking_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  # D = 0 variance is 2/64.
  # Reference component is b D b' = 5.2.
  # Total variance is (2 + 5.2) / 64 = 9/80.
  expect_equal(res$variance, 9 / 80, tolerance = 1e-12)
})

test_that("multi_estimate variance matches raking in one-block nonzero-D case", {
  inp <- make_est_inputs_nonzero_D()

  res_multi <- multi_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  res_raking <- raking_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  expect_equal(res_multi$variance, res_raking$variance, tolerance = 1e-12)
  expect_equal(res_multi$variance, 9 / 80, tolerance = 1e-12)
})

test_that("alp_estimate variance includes nonzero D component", {
  inp <- make_est_inputs_nonzero_D()

  res <- alp_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  # D = 0 variance is 89/288.
  # Reference component is 5.2 / 64.
  # Total variance is 281/720.
  expect_equal(res$variance, 281 / 720, tolerance = 1e-12)
})

test_that("clw_estimate variance includes nonzero D component", {
  inp <- make_est_inputs_nonzero_D()

  res <- clw_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )

  # D = 0 variance is 49/128.
  # Reference component is 1.3 / 64.
  # Total variance is 129/320.
  expect_equal(res$variance, 129 / 320, tolerance = 1e-12)
})


# method differences ----

test_that("estimators have expected variance relationships in the synthetic example", {
  inp <- make_est_inputs()

  v_raking <- raking_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )$variance

  v_multi <- multi_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )$variance

  v_alp <- alp_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )$variance

  v_clw <- clw_estimate(
    inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta
  )$variance

  expect_equal(v_raking, v_multi, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(v_raking, v_alp)))
  expect_false(isTRUE(all.equal(v_raking, v_clw)))
  expect_false(isTRUE(all.equal(v_alp, v_clw)))
})


# dispatch_estimator_one_domain routing ----

test_that("dispatch_estimator_one_domain routes alp correctly", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("alp", inp)

  expect_equal(
    dispatch_estimator_one_domain(build, inp$Y, inp$Z, inp$w, inp$X),
    alp_estimate(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)
  )
})

test_that("dispatch_estimator_one_domain routes clw correctly", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("clw", inp)

  expect_equal(
    dispatch_estimator_one_domain(build, inp$Y, inp$Z, inp$w, inp$X),
    clw_estimate(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)
  )
})

test_that("dispatch_estimator_one_domain routes calibration correctly", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("calibration", inp)

  expect_equal(
    dispatch_estimator_one_domain(build, inp$Y, inp$Z, inp$w, inp$X),
    raking_estimate(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)
  )
})

test_that("dispatch_estimator_one_domain treats cali as calibration", {
  inp    <- make_est_inputs()
  build1 <- make_dispatch_build("calibration", inp)
  build2 <- make_dispatch_build("cali", inp)

  expect_equal(
    dispatch_estimator_one_domain(build1, inp$Y, inp$Z, inp$w, inp$X),
    dispatch_estimator_one_domain(build2, inp$Y, inp$Z, inp$w, inp$X)
  )
})

test_that("dispatch_estimator_one_domain routes multi correctly", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("multi", inp)

  expect_equal(
    dispatch_estimator_one_domain(build, inp$Y, inp$Z, inp$w, inp$X),
    multi_estimate(inp$Y, inp$Z, inp$w, inp$X, inp$D, inp$S_beta)
  )
})

test_that("dispatch_estimator_one_domain method matching is case-insensitive", {
  inp    <- make_est_inputs()
  build1 <- make_dispatch_build("alp", inp)
  build2 <- make_dispatch_build("ALP", inp)

  expect_equal(
    dispatch_estimator_one_domain(build1, inp$Y, inp$Z, inp$w, inp$X),
    dispatch_estimator_one_domain(build2, inp$Y, inp$Z, inp$w, inp$X)
  )
})

test_that("dispatch_estimator_one_domain errors for unknown method", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("unknown_method", inp)

  expect_error(
    dispatch_estimator_one_domain(build, inp$Y, inp$Z, inp$w, inp$X),
    "Unknown method 'unknown_method'",
    fixed = TRUE
  )
})

test_that("dispatch_estimator_one_domain errors when Y and zvec lengths differ", {
  inp   <- make_est_inputs()
  build <- make_dispatch_build("alp", inp)

  expect_error(
    dispatch_estimator_one_domain(
      build = build,
      Y = inp$Y,
      zvec = c(1L, 1L),
      w = inp$w,
      X = inp$X
    ),
    "Length mismatch between `Y` and `zvec`.",
    fixed = TRUE
  )
})


# dispatch_estimator domain routing ----

test_that("dispatch_estimator handles overall mode", {
  build   <- make_dispatch_build("alp")
  yz_data <- make_yz_data(domain_overall)

  res <- dispatch_estimator(build, yz_data)

  expect_equal(res$type, "single")
  expect_equal(res$labels, "Overall")
  expect_named(res$estimates, c("mean", "variance"))

  expect_equal(res$estimates$mean, 3.5, tolerance = 1e-12)
})

test_that("dispatch_estimator handles binary domain mode", {
  build   <- make_dispatch_build("alp")
  yz_data <- make_yz_data(domain_binary)

  res <- dispatch_estimator(build, yz_data)

  expect_equal(res$type, "single")
  expect_equal(res$labels, "grp = 1")
  expect_named(res$estimates, c("mean", "variance"))

  # Binary domain uses observations 1 and 2.
  expect_equal(res$estimates$mean, 1.5, tolerance = 1e-12)
})

test_that("dispatch_estimator handles factor domain mode", {
  build   <- make_dispatch_build("alp")
  yz_data <- make_yz_data(domain_factor)

  res <- dispatch_estimator(build, yz_data)

  expect_equal(res$type, "multi")
  expect_equal(res$labels, c("A", "B"))
  expect_length(res$estimates, 2L)

  expect_named(res$estimates[[1]], c("mean", "variance"))
  expect_named(res$estimates[[2]], c("mean", "variance"))

  # A uses observations 1 and 3.
  # B uses observations 2 and 4.
  expect_equal(res$estimates[[1]]$mean, 2.5, tolerance = 1e-12)
  expect_equal(res$estimates[[2]]$mean, 4.5, tolerance = 1e-12)
})

test_that("dispatch_estimator errors for unsupported domain mode", {
  build <- make_dispatch_build("alp")

  yz_data <- make_yz_data(
    list(
      mode = "unknown",
      labels = NULL,
      indicators = NULL
    )
  )

  expect_error(
    dispatch_estimator(build, yz_data),
    "Unsupported domain mode in `yz_data$domain`.",
    fixed = TRUE
  )
})
