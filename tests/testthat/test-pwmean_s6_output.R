# helpers ----
#
# Synthetic inputs with known analytical results.
#
# z975 = qnorm(0.975) is used for 95% CI half-width.
#
# Single-domain inputs:
#   naive: mean = 3.5, variance = 1.75  -> se = sqrt(1.75)
#   est  : mean = 4.0, variance = 0.25  -> se = 0.5
#
# Multi-domain inputs (labels = c("A", "B")):
#   naive A: mean = 2.5, variance = 2.25  -> se = 1.5
#   naive B: mean = 4.5, variance = 6.25  -> se = 2.5
#   est   A: mean = 3.0, variance = 1.00  -> se = 1.0
#   est   B: mean = 5.0, variance = 4.00  -> se = 2.0

z975 <- stats::qnorm(0.975)

make_build <- function(method = "alp") {
  list(method = method)
}

make_na_info <- function(action_class = "omit") {
  list(na_action = structure(integer(0), class = action_class))
}

make_single_est <- function(mean = 4.0, variance = 0.25) {
  list(type = "single", labels = "Overall",
       estimates = list(mean = mean, variance = variance))
}

make_single_naive <- function(mean = 3.5, variance = 1.75) {
  list(type = "single", labels = "Overall",
       estimates = list(mean = mean, variance = variance))
}

make_multi_est <- function() {
  list(
    type   = "multi",
    labels = c("A", "B"),
    estimates = list(
      list(mean = 3.0, variance = 1.0),
      list(mean = 5.0, variance = 4.0)
    )
  )
}

make_multi_naive <- function() {
  list(
    type   = "multi",
    labels = c("A", "B"),
    estimates = list(
      list(mean = 2.5, variance = 2.25),
      list(mean = 4.5, variance = 6.25)
    )
  )
}


# build_domains_df: output structure ----

test_that("build_domains_df: returns a data.frame", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = sqrt(1.75),
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_s3_class(res, "data.frame")
})

test_that("build_domains_df: column names and order match the spec", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = sqrt(1.75),
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_equal(
    names(res),
    c("domain",
      "unweighted_mean", "unweighted_se", "unweighted_lower", "unweighted_upper",
      "adjusted_mean",   "adjusted_se",   "adjusted_lower",   "adjusted_upper")
  )
})

test_that("build_domains_df: nrow equals length(labels)", {
  res <- build_domains_df(
    labels   = c("A", "B"),
    mean_unw = c(2.5, 4.5), se_unw = c(1.5, 2.5),
    mean_adj = c(3.0, 5.0), se_adj = c(1.0, 2.0)
  )
  expect_equal(nrow(res), 2L)
})

test_that("build_domains_df: domain column is character (not factor)", {
  res <- build_domains_df(
    labels   = c("A", "B"),
    mean_unw = c(2.5, 4.5), se_unw = c(1.5, 2.5),
    mean_adj = c(3.0, 5.0), se_adj = c(1.0, 2.0)
  )
  expect_type(res$domain, "character")
})


# build_domains_df: value correctness ----

test_that("build_domains_df: mean and se columns equal the inputs", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = sqrt(1.75),
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_equal(res$unweighted_mean, 3.5)
  expect_equal(res$unweighted_se,   sqrt(1.75))
  expect_equal(res$adjusted_mean,   4.0)
  expect_equal(res$adjusted_se,     0.5)
})

test_that("build_domains_df: unweighted CI equals mean +/- qnorm(0.975) * se", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = sqrt(1.75),
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_equal(res$unweighted_lower, 3.5 - z975 * sqrt(1.75))
  expect_equal(res$unweighted_upper, 3.5 + z975 * sqrt(1.75))
})

test_that("build_domains_df: adjusted CI equals mean +/- qnorm(0.975) * se", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = sqrt(1.75),
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_equal(res$adjusted_lower, 4.0 - z975 * 0.5)
  expect_equal(res$adjusted_upper, 4.0 + z975 * 0.5)
})

test_that("build_domains_df: CIs vectorize correctly over multiple rows", {
  res <- build_domains_df(
    labels   = c("A", "B"),
    mean_unw = c(2.5, 4.5), se_unw = c(1.5, 2.5),
    mean_adj = c(3.0, 5.0), se_adj = c(1.0, 2.0)
  )
  expect_equal(res$unweighted_lower, c(2.5, 4.5) - z975 * c(1.5, 2.5))
  expect_equal(res$unweighted_upper, c(2.5, 4.5) + z975 * c(1.5, 2.5))
  expect_equal(res$adjusted_lower,   c(3.0, 5.0) - z975 * c(1.0, 2.0))
  expect_equal(res$adjusted_upper,   c(3.0, 5.0) + z975 * c(1.0, 2.0))
})

test_that("build_domains_df: NA in se propagates to lower and upper", {
  res <- build_domains_df(
    labels   = "Overall",
    mean_unw = 3.5, se_unw = NA_real_,
    mean_adj = 4.0, se_adj = 0.5
  )
  expect_true(is.na(res$unweighted_lower))
  expect_true(is.na(res$unweighted_upper))
  expect_false(is.na(res$adjusted_lower))
  expect_false(is.na(res$adjusted_upper))
})

test_that("build_domains_df: domain column preserves the label vector", {
  res <- build_domains_df(
    labels   = c("A", "B"),
    mean_unw = c(2.5, 4.5), se_unw = c(1.5, 2.5),
    mean_adj = c(3.0, 5.0), se_adj = c(1.0, 2.0)
  )
  expect_equal(res$domain, c("A", "B"))
})


# assemble_output: single-result case ----

test_that("assemble_output: single-result returns list with method, estimates, na.action", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(),
    naive   = make_single_naive(),
    na_info = make_na_info("omit")
  )
  expect_named(res, c("method", "estimates", "na.action"))
})

test_that("assemble_output: single-result method propagates from build", {
  res <- assemble_output(
    build   = make_build("calibration"),
    est     = make_single_est(),
    naive   = make_single_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$method, "calibration")
})

test_that("assemble_output: single-result na.action propagates from na_info", {
  na_info <- make_na_info("exclude")
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(),
    naive   = make_single_naive(),
    na_info = na_info
  )
  expect_identical(res$na.action, na_info$na_action)
})

test_that("assemble_output: single-result estimates is a one-row data.frame", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(),
    naive   = make_single_naive(),
    na_info = make_na_info("omit")
  )
  expect_s3_class(res$estimates, "data.frame")
  expect_equal(nrow(res$estimates), 1L)
})

test_that("assemble_output: single-result domain label equals naive$labels", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(),
    naive   = make_single_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$domain, "Overall")
})

test_that("assemble_output: single-result unweighted mean and se come from naive", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(),
    naive   = make_single_naive(mean = 3.5, variance = 1.75),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$unweighted_mean, 3.5)
  expect_equal(res$estimates$unweighted_se,   sqrt(1.75))
})

test_that("assemble_output: single-result adjusted mean and se come from est", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(mean = 4.0, variance = 0.25),
    naive   = make_single_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$adjusted_mean, 4.0)
  expect_equal(res$estimates$adjusted_se,   0.5)
})

test_that("assemble_output: single-result CIs equal mean +/- qnorm(0.975) * se", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_single_est(mean = 4.0, variance = 0.25),
    naive   = make_single_naive(mean = 3.5, variance = 1.75),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$unweighted_lower, 3.5 - z975 * sqrt(1.75))
  expect_equal(res$estimates$unweighted_upper, 3.5 + z975 * sqrt(1.75))
  expect_equal(res$estimates$adjusted_lower,   4.0 - z975 * 0.5)
  expect_equal(res$estimates$adjusted_upper,   4.0 + z975 * 0.5)
})


# assemble_output: multi-result case ----

test_that("assemble_output: multi-result returns list with method, estimates, na.action", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_named(res, c("method", "estimates", "na.action"))
})

test_that("assemble_output: multi-result estimates has one row per domain", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(nrow(res$estimates), 2L)
})

test_that("assemble_output: multi-result domain column equals est$labels", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$domain, c("A", "B"))
})

test_that("assemble_output: multi-result unweighted columns come from naive estimates", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$unweighted_mean, c(2.5, 4.5))
  expect_equal(res$estimates$unweighted_se,   c(1.5, 2.5))
})

test_that("assemble_output: multi-result adjusted columns come from est estimates", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$adjusted_mean, c(3.0, 5.0))
  expect_equal(res$estimates$adjusted_se,   c(1.0, 2.0))
})

test_that("assemble_output: multi-result CIs equal mean +/- qnorm(0.975) * se", {
  res <- assemble_output(
    build   = make_build("alp"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$estimates$unweighted_lower, c(2.5, 4.5) - z975 * c(1.5, 2.5))
  expect_equal(res$estimates$unweighted_upper, c(2.5, 4.5) + z975 * c(1.5, 2.5))
  expect_equal(res$estimates$adjusted_lower,   c(3.0, 5.0) - z975 * c(1.0, 2.0))
  expect_equal(res$estimates$adjusted_upper,   c(3.0, 5.0) + z975 * c(1.0, 2.0))
})

test_that("assemble_output: multi-result method propagates from build", {
  res <- assemble_output(
    build   = make_build("multi"),
    est     = make_multi_est(),
    naive   = make_multi_naive(),
    na_info = make_na_info("omit")
  )
  expect_equal(res$method, "multi")
})


# assemble_output: input validation ----

test_that("assemble_output: errors when est$type is NULL", {
  bad_est        <- make_single_est()
  bad_est$type   <- NULL
  expect_error(
    assemble_output(
      build = make_build(), est = bad_est, naive = make_single_naive(),
      na_info = make_na_info()
    ),
    "`est` and `naive` must both contain a `type` field.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors when naive$type is NULL", {
  bad_naive       <- make_single_naive()
  bad_naive$type  <- NULL
  expect_error(
    assemble_output(
      build = make_build(), est = make_single_est(), naive = bad_naive,
      na_info = make_na_info()
    ),
    "`est` and `naive` must both contain a `type` field.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors when est$type and naive$type differ", {
  expect_error(
    assemble_output(
      build = make_build(), est = make_single_est(), naive = make_multi_naive(),
      na_info = make_na_info()
    ),
    "`est$type` and `naive$type` do not match.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors when est$labels and est$estimates differ in length", {
  bad_est            <- make_multi_est()
  bad_est$estimates  <- bad_est$estimates[1]   # length 1, labels still length 2
  expect_error(
    assemble_output(
      build = make_build(), est = bad_est, naive = make_multi_naive(),
      na_info = make_na_info()
    ),
    "Length mismatch between `est$labels` and `est$estimates`.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors when naive$labels and naive$estimates differ in length", {
  bad_naive             <- make_multi_naive()
  bad_naive$estimates   <- bad_naive$estimates[1]
  expect_error(
    assemble_output(
      build = make_build(), est = make_multi_est(), naive = bad_naive,
      na_info = make_na_info()
    ),
    "Length mismatch between `naive$labels` and `naive$estimates`.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors when multi est$labels and naive$labels do not match", {
  bad_naive          <- make_multi_naive()
  bad_naive$labels   <- c("X", "Y")
  expect_error(
    assemble_output(
      build = make_build(), est = make_multi_est(), naive = bad_naive,
      na_info = make_na_info()
    ),
    "`est$labels` and `naive$labels` do not match.",
    fixed = TRUE
  )
})

test_that("assemble_output: errors on unsupported type value", {
  bad_est         <- make_single_est()
  bad_est$type    <- "weird"
  bad_naive       <- make_single_naive()
  bad_naive$type  <- "weird"
  expect_error(
    assemble_output(
      build = make_build(), est = bad_est, naive = bad_naive,
      na_info = make_na_info()
    ),
    "Unsupported result type in `est$type` / `naive$type`.",
    fixed = TRUE
  )
})
