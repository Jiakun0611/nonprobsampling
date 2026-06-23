# helpers ----
#
# Synthetic data frame with known analytical results.
#
# y = c(1, 2, 4, 7)
#
# overall:
#   mean     = (1+2+4+7)/4 = 3.5
#   variance = var(c(1,2,4,7)) / 4 = 7 / 4 = 1.75
#
# grp_bin (indicator z = c(1,1,0,0)):
#   y_sub = c(1, 2),  mean = 1.5
#   variance = var(c(1,2)) / 2 = 0.5 / 2 = 0.25
#
# grp_fac / grp_chr level A (obs 1, 3): y_sub = c(1, 4)
#   mean = 2.5,  variance = var(c(1,4)) / 2 = 4.5 / 2 = 2.25
#
# grp_fac / grp_chr level B (obs 2, 4): y_sub = c(2, 7)
#   mean = 4.5,  variance = var(c(2,7)) / 2 = 12.5 / 2 = 6.25

make_naive_df <- function() {
  data.frame(
    y       = c(1.0, 2.0, 4.0, 7.0),
    grp_bin = c(1L, 1L, 0L, 0L),
    grp_fac = factor(c("A", "B", "A", "B")),
    grp_chr = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
}

df <- make_naive_df()


# naive_mean_one_domain: output structure ----

test_that("naive_mean_one_domain: returns a named list with mean and variance", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7))
  expect_type(res, "list")
  expect_named(res, c("mean", "variance"))
})

test_that("naive_mean_one_domain: mean and variance are length-1 scalars", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7))
  expect_length(res$mean,     1L)
  expect_length(res$variance, 1L)
})


# naive_mean_one_domain: zvec = NULL (overall) ----

test_that("naive_mean_one_domain: zvec = NULL uses all observations for the mean", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7))
  expect_equal(res$mean, 3.5)
})

test_that("naive_mean_one_domain: zvec = NULL gives variance = var(y) / n", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7))
  expect_equal(res$variance, 1.75)
})


# naive_mean_one_domain: domain subset ----

test_that("naive_mean_one_domain: zvec = c(1,1,0,0) selects the first two observations", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7), zvec = c(1L, 1L, 0L, 0L))
  expect_equal(res$mean, 1.5)
})

test_that("naive_mean_one_domain: zvec = c(1,1,0,0) gives variance = var(c(1,2)) / 2", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7), zvec = c(1L, 1L, 0L, 0L))
  expect_equal(res$variance, 0.25)
})


# naive_mean_one_domain: edge cases ----

test_that("naive_mean_one_domain: empty domain (all zeros) returns NA mean and NA variance", {
  res <- naive_mean_one_domain(yvec = c(1, 2, 4, 7), zvec = c(0L, 0L, 0L, 0L))
  expect_true(is.na(res$mean))
  expect_true(is.na(res$variance))
})

test_that("naive_mean_one_domain: single observation returns valid mean and NA variance", {
  res <- naive_mean_one_domain(yvec = c(5.0), zvec = c(1L))
  expect_equal(res$mean, 5.0)
  expect_true(is.na(res$variance))
})


# naive_mean_one_domain: input validation ----

test_that("naive_mean_one_domain: rejects non-numeric yvec", {
  expect_error(
    naive_mean_one_domain(yvec = c("a", "b")),
    "`yvec` must be numeric.",
    fixed = TRUE
  )
})

test_that("naive_mean_one_domain: rejects zvec with different length than yvec", {
  expect_error(
    naive_mean_one_domain(yvec = c(1, 2, 4, 7), zvec = c(1L, 0L)),
    "Length mismatch between `yvec` and `zvec`.",
    fixed = TRUE
  )
})


# naive_mean: output structure (overall) ----

test_that("naive_mean: overall returns list with type, labels, estimates", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_named(res, c("type", "labels", "estimates"))
})

test_that("naive_mean: overall returns type = 'single'", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_equal(res$type, "single")
})

test_that("naive_mean: overall returns label 'Overall'", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_equal(res$labels, "Overall")
})

test_that("naive_mean: overall estimates has mean and variance", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_named(res$estimates, c("mean", "variance"))
})


# naive_mean: mean and variance correctness (overall) ----

test_that("naive_mean: overall mean equals 3.5", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_equal(res$estimates$mean, 3.5)
})

test_that("naive_mean: overall variance equals 1.75", {
  res <- naive_mean(df, domain_var = NULL, y = "y")
  expect_equal(res$estimates$variance, 1.75)
})


# naive_mean: binary domain ----

test_that("naive_mean: binary domain returns type = 'single'", {
  res <- naive_mean(df, domain_var = "grp_bin", y = "y")
  expect_equal(res$type, "single")
})

test_that("naive_mean: binary domain returns label 'grp_bin = 1'", {
  res <- naive_mean(df, domain_var = "grp_bin", y = "y")
  expect_equal(res$labels, "grp_bin = 1")
})

test_that("naive_mean: binary domain estimates has mean and variance", {
  res <- naive_mean(df, domain_var = "grp_bin", y = "y")
  expect_named(res$estimates, c("mean", "variance"))
})

test_that("naive_mean: binary domain mean equals 1.5", {
  res <- naive_mean(df, domain_var = "grp_bin", y = "y")
  expect_equal(res$estimates$mean, 1.5)
})

test_that("naive_mean: binary domain variance equals 0.25", {
  res <- naive_mean(df, domain_var = "grp_bin", y = "y")
  expect_equal(res$estimates$variance, 0.25)
})


# naive_mean: factor domain ----

test_that("naive_mean: factor domain returns type = 'multi'", {
  res <- naive_mean(df, domain_var = "grp_fac", y = "y")
  expect_equal(res$type, "multi")
})

test_that("naive_mean: factor domain has two labels and two estimates", {
  res <- naive_mean(df, domain_var = "grp_fac", y = "y")
  expect_length(res$labels,    2L)
  expect_length(res$estimates, 2L)
})

test_that("naive_mean: factor domain labels include the domain variable name", {
  res <- naive_mean(df, domain_var = "grp_fac", y = "y")
  expect_equal(res$labels, c("grp_fac = A", "grp_fac = B"))
})

test_that("naive_mean: factor domain each estimate has mean and variance", {
  res <- naive_mean(df, domain_var = "grp_fac", y = "y")
  for (est in res$estimates) {
    expect_named(est, c("mean", "variance"))
  }
})

test_that("naive_mean: factor domain level A mean equals 2.5", {
  res   <- naive_mean(df, domain_var = "grp_fac", y = "y")
  idx_A <- which(res$labels == "grp_fac = A")
  expect_equal(res$estimates[[idx_A]]$mean, 2.5)
})

test_that("naive_mean: factor domain level A variance equals 2.25", {
  res   <- naive_mean(df, domain_var = "grp_fac", y = "y")
  idx_A <- which(res$labels == "grp_fac = A")
  expect_equal(res$estimates[[idx_A]]$variance, 2.25)
})

test_that("naive_mean: factor domain level B mean equals 4.5", {
  res   <- naive_mean(df, domain_var = "grp_fac", y = "y")
  idx_B <- which(res$labels == "grp_fac = B")
  expect_equal(res$estimates[[idx_B]]$mean, 4.5)
})

test_that("naive_mean: factor domain level B variance equals 6.25", {
  res   <- naive_mean(df, domain_var = "grp_fac", y = "y")
  idx_B <- which(res$labels == "grp_fac = B")
  expect_equal(res$estimates[[idx_B]]$variance, 6.25)
})


# naive_mean: character domain ----

test_that("naive_mean: character domain returns type = 'multi'", {
  res <- naive_mean(df, domain_var = "grp_chr", y = "y")
  expect_equal(res$type, "multi")
})

test_that("naive_mean: character domain has two labels and two estimates", {
  res <- naive_mean(df, domain_var = "grp_chr", y = "y")
  expect_length(res$labels,    2L)
  expect_length(res$estimates, 2L)
})

test_that("naive_mean: character domain labels include the domain variable name", {
  res <- naive_mean(df, domain_var = "grp_chr", y = "y")
  expect_equal(res$labels, c("grp_chr = A", "grp_chr = B"))
})

test_that("naive_mean: character domain each estimate has mean and variance", {
  res <- naive_mean(df, domain_var = "grp_chr", y = "y")
  for (est in res$estimates) {
    expect_named(est, c("mean", "variance"))
  }
})


# naive_mean: NA filtering ----

test_that("naive_mean: NA in y is silently dropped before computing mean", {
  df_na       <- make_naive_df()
  df_na$y[1L] <- NA_real_
  # After dropping obs 1, y = c(2, 4, 7), overall mean = 13/3
  res <- naive_mean(df_na, domain_var = NULL, y = "y")
  expect_equal(res$estimates$mean, 13 / 3)
})

test_that("naive_mean: NA in domain_var is silently dropped before computing", {
  df_na             <- make_naive_df()
  df_na$grp_fac[1L] <- NA
  # Obs 1 dropped; level A keeps only obs 3 (y=4): single obs -> NA variance
  res   <- naive_mean(df_na, domain_var = "grp_fac", y = "y")
  idx_A <- which(res$labels == "grp_fac = A")
  expect_equal(res$estimates[[idx_A]]$mean, 4.0)
  expect_true(is.na(res$estimates[[idx_A]]$variance))
})


# naive_mean: input validation ----

test_that("naive_mean: rejects non-data.frame df", {
  expect_error(
    naive_mean(list(y = c(1, 2)), domain_var = NULL, y = "y"),
    "`df` must be a data.frame.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects non-character y", {
  expect_error(
    naive_mean(df, domain_var = NULL, y = 1L),
    "`y` must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects NA y", {
  expect_error(
    naive_mean(df, domain_var = NULL, y = NA_character_),
    "`y` must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects empty string y", {
  expect_error(
    naive_mean(df, domain_var = NULL, y = ""),
    "`y` must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects y not found in df", {
  expect_error(
    naive_mean(df, domain_var = NULL, y = "not_a_col"),
    "Outcome variable 'not_a_col' not found in `df`.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects non-character domain_var", {
  expect_error(
    naive_mean(df, domain_var = 1L, y = "y"),
    "`domain_var` must be NULL or a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects empty string domain_var", {
  expect_error(
    naive_mean(df, domain_var = "", y = "y"),
    "`domain_var` must be NULL or a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("naive_mean: rejects domain_var not found in df", {
  expect_error(
    naive_mean(df, domain_var = "not_a_col", y = "y"),
    "Domain variable 'not_a_col' not found in `df`.",
    fixed = TRUE
  )
})
