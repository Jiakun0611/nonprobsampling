# Setup ----

set.seed(123)

# sp1: factor a (shared), numeric x (unique to sp1), wt1 — 150 rows (larger)
# sp2: factor a (shared), factor b (unique to sp2), wt2 — 100 rows (smaller)
# Shared model-matrix columns after expansion: aB, aC
# sp1-only column: x
# sp2-only column: b

sp1_raw <- data.frame(
  a   = factor(sample(c("A","B","C"), 150, replace = TRUE), levels = c("A","B","C")),
  x   = rnorm(150),
  wt1 = runif(150, 10, 100)
)

sp2_raw <- data.frame(
  a   = factor(sample(c("A","B","C"), 100, replace = TRUE), levels = c("A","B","C")),
  b   = factor(sample(c("P","Q"),     100, replace = TRUE), levels = c("P","Q")),
  wt2 = runif(100, 10, 100)
)

sp1_new <- sp1_raw
sp2_new <- sp2_raw


# Return structure ----

test_that("precal_cumulative_order: returns correct list names", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_named(out, c("sp_new", "total_vector", "log_messages", "order_used"))
})

test_that("precal_cumulative_order: sp_new has same length as input", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_length(out$sp_new, 2)
})

test_that("precal_cumulative_order: total_vector starts with (Intercept)", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_equal(names(out$total_vector)[1], "(Intercept)")
})

test_that("precal_cumulative_order: log_messages is a character vector", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_type(out$log_messages, "character")
})


# order_used ----

test_that("precal_cumulative_order: sp_order = 'size' keeps larger sample first when already first", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_equal(out$order_used[1], 1L)
})

test_that("precal_cumulative_order: sp_order = 'size' reorders when smaller is listed first", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp2_raw, sp1_raw),
    sp_new   = list(sp2_new, sp1_new),
    weight   = c("wt2", "wt1"),
    sp_order = "size"
  )
  expect_equal(out$order_used[1], 2L)
})

test_that("precal_cumulative_order: sp_order = 'given' uses list order", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "given"
  )
  expect_equal(out$order_used, 1:2)
})

test_that("precal_cumulative_order: order_used length equals number of samples", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_length(out$order_used, 2L)
})


# Reference sample unchanged ----

test_that("precal_cumulative_order: reference sample weights are not modified", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_equal(out$sp_new[[1]][["wt1"]], sp1_raw[["wt1"]])
})


# Calibration correctness ----

test_that("precal_cumulative_order: calibrated total weight matches reference total (intercept)", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  wt2_cal <- out$sp_new[[2]][["wt2"]]
  expect_equal(sum(wt2_cal), unname(out$total_vector["(Intercept)"]), tolerance = 1e-5)
})

test_that("precal_cumulative_order: calibrated weighted totals match target for shared columns", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  wt2_cal <- out$sp_new[[2]][["wt2"]]
  X2 <- model.matrix(~ ., data = sp2_raw[, c("a", "b")])[, -1, drop = FALSE]

  for (col in c("aB", "aC")) {
    expect_equal(
      sum(wt2_cal * X2[, col]),
      unname(out$total_vector[col]),
      tolerance = 1e-5
    )
  }
})


# total_vector grows with new columns ----

test_that("precal_cumulative_order: total_vector includes unique columns from sp2", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_true("bQ" %in% names(out$total_vector))
})

test_that("precal_cumulative_order: total_vector contains all columns from both samples", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_true(all(c("(Intercept)", "aB", "aC", "x", "bQ") %in% names(out$total_vector)))
})


# No shared columns ----

test_that("precal_cumulative_order: calibrates intercept only when no columns are shared", {
  set.seed(99)
  sp_a <- data.frame(x = rnorm(100), wt1 = runif(100, 10, 100))
  sp_b <- data.frame(y = rnorm(80),  wt2 = runif(80,  10, 100))

  out <- precal_cumulative_order(
    sp_raw   = list(sp_a, sp_b),
    sp_new   = list(sp_a, sp_b),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )

  wt_b_cal <- out$sp_new[[2]][["wt2"]]
  expect_equal(sum(wt_b_cal), unname(out$total_vector["(Intercept)"]), tolerance = 1e-5)
})

test_that("precal_cumulative_order: total_vector contains columns from both samples when no overlap", {
  set.seed(99)
  sp_a <- data.frame(x = rnorm(100), wt1 = runif(100, 10, 100))
  sp_b <- data.frame(y = rnorm(80),  wt2 = runif(80,  10, 100))

  out <- precal_cumulative_order(
    sp_raw   = list(sp_a, sp_b),
    sp_new   = list(sp_a, sp_b),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )

  expect_true(all(c("x", "y") %in% names(out$total_vector)))
})


# List names kept through reordering ----

test_that("precal_cumulative_order: sp_new names kept with sp_order = 'given'", {
  out <- precal_cumulative_order(
    sp_raw   = list(survey_a = sp1_raw, survey_b = sp2_raw),
    sp_new   = list(survey_a = sp1_new, survey_b = sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "given"
  )
  expect_equal(names(out$sp_new), c("survey_a", "survey_b"))
})

test_that("precal_cumulative_order: sp_new names kept after size reorder", {
  out <- precal_cumulative_order(
    sp_raw   = list(survey_a = sp2_raw, survey_b = sp1_raw),
    sp_new   = list(survey_a = sp2_new, survey_b = sp1_new),
    weight   = c("wt2", "wt1"),
    sp_order = "size"
  )
  expect_equal(names(out$sp_new), c("survey_a", "survey_b"))
})


# log_messages content ----

test_that("precal_cumulative_order: log_messages has one entry per sample", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_length(out$log_messages, 2)
})

test_that("precal_cumulative_order: log_messages mentions non-calibrated sample", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_true(any(grepl("Non-calibrated sample", out$log_messages)))
})

test_that("precal_cumulative_order: log_messages mentions calibrated sample", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_true(any(grepl("Calibrated sample", out$log_messages)))
})

test_that("precal_cumulative_order: log_messages notes 'largest' when sp_order = 'size'", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "size"
  )
  expect_true(any(grepl("largest", out$log_messages)))
})

test_that("precal_cumulative_order: log_messages notes 'first' when sp_order = 'given'", {
  out <- precal_cumulative_order(
    sp_raw   = list(sp1_raw, sp2_raw),
    sp_new   = list(sp1_new, sp2_new),
    weight   = c("wt1", "wt2"),
    sp_order = "given"
  )
  expect_true(any(grepl("first", out$log_messages)))
})
