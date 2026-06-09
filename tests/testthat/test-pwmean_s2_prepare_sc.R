# Setup ----

data(sc)

# helper: build a minimal pw_fit around a data frame
make_pw_fit <- function(raw_sc, weights = NULL, keep_sc = NULL, Xc = NULL) {
  n      <- nrow(raw_sc)
  if (is.null(keep_sc)) keep_sc <- rep(TRUE, n)
  n_keep <- sum(keep_sc)
  if (is.null(weights)) weights <- rep(1.0, n_keep)
  if (is.null(Xc))      Xc      <- matrix(1.0, nrow = n_keep, ncol = 1L)
  structure(
    list(
      pseudo_weights = weights,
      internal = list(
        raw_sc = raw_sc,
        na     = list(keep_sc = keep_sc),
        Xc     = Xc
      )
    ),
    class = "pw_fit"
  )
}

fit <- make_pw_fit(sc)


# clean path ----

test_that("prepare_sc_data: returns named list with sc, X, w, idx_keep", {
  res <- prepare_sc_data(fit)
  expect_named(res, c("sc", "X", "w", "idx_keep"))
})

test_that("prepare_sc_data: sc has same columns as raw_sc", {
  res <- prepare_sc_data(fit)
  expect_equal(names(res$sc), names(sc))
})

test_that("prepare_sc_data: sc row count equals sum(keep_sc)", {
  keep         <- rep(TRUE, nrow(sc))
  keep[1:10]   <- FALSE
  fit_subset   <- make_pw_fit(sc, keep_sc = keep)
  res          <- prepare_sc_data(fit_subset)
  expect_equal(nrow(res$sc), sum(keep))
})

test_that("prepare_sc_data: idx_keep matches which(keep_sc)", {
  keep         <- rep(TRUE, nrow(sc))
  keep[c(1, 5, 10)] <- FALSE
  fit_subset   <- make_pw_fit(sc, keep_sc = keep)
  res          <- prepare_sc_data(fit_subset)
  expect_equal(res$idx_keep, which(keep))
})

test_that("prepare_sc_data: sc rows correspond to keep_sc indices", {
  keep         <- rep(c(TRUE, FALSE), length.out = nrow(sc))
  fit_subset   <- make_pw_fit(sc, keep_sc = keep)
  res          <- prepare_sc_data(fit_subset)
  expect_equal(res$sc, sc[keep, , drop = FALSE], ignore_attr = TRUE)
})

test_that("prepare_sc_data: X is returned unchanged from build$internal$Xc", {
  Xc           <- matrix(seq_len(2 * nrow(sc)), nrow = nrow(sc), ncol = 2L)
  fit_with_Xc  <- make_pw_fit(sc, Xc = Xc)
  res          <- prepare_sc_data(fit_with_Xc)
  expect_equal(res$X, Xc)
})

test_that("prepare_sc_data: w length equals nrow(X) when no NA weights", {
  res <- prepare_sc_data(fit)
  expect_equal(length(res$w), nrow(res$X))
})


# na.exclude: NA weights removed ----

test_that("prepare_sc_data: NA weights from na.exclude builds are removed", {
  n             <- nrow(sc)
  keep          <- rep(TRUE, n)
  keep[1:5]     <- FALSE
  w_full        <- rep(1.0, n)
  w_full[!keep] <- NA_real_
  fit_excl      <- make_pw_fit(sc, weights = w_full, keep_sc = keep,
                               Xc = matrix(1.0, nrow = sum(keep), ncol = 1L))
  res           <- prepare_sc_data(fit_excl)
  expect_false(anyNA(res$w))
  expect_equal(length(res$w), sum(keep))
})


# type guards ----

test_that("prepare_sc_data: rejects non-logical keep_sc", {
  bad                      <- fit
  bad$internal$na$keep_sc  <- seq_len(nrow(sc))
  expect_error(
    prepare_sc_data(bad),
    "prepare_sc_data: keep_sc must be a logical vector.",
    fixed = TRUE
  )
})

test_that("prepare_sc_data: rejects vector Xc (nrow is NULL)", {
  bad              <- fit
  bad$internal$Xc  <- rep(1.0, nrow(sc))
  expect_error(
    prepare_sc_data(bad),
    "prepare_sc_data: nrow(build$Xc) is NULL",
    fixed = TRUE
  )
})


# length mismatch checks ----

test_that("prepare_sc_data: errors on nrow(X) vs length(w) mismatch", {
  n      <- nrow(sc)
  bad    <- make_pw_fit(sc,
                        weights = rep(1.0, n),
                        Xc      = matrix(1.0, nrow = n + 5L, ncol = 1L))
  expect_error(
    prepare_sc_data(bad),
    "prepare_sc_data: mismatch between nrow(build$Xc) and length(build$pseudo_weights).",
    fixed = TRUE
  )
})

test_that("prepare_sc_data: errors on nrow(sc_keep) vs length(w) mismatch", {
  n      <- nrow(sc)
  keep   <- rep(TRUE, n)
  keep[1:5] <- FALSE
  # weights and Xc sized to n (not n_keep), so sc_keep length differs from w
  bad    <- make_pw_fit(sc,
                        weights = rep(1.0, n),
                        keep_sc = keep,
                        Xc      = matrix(1.0, nrow = n, ncol = 1L))
  expect_error(
    prepare_sc_data(bad),
    "prepare_sc_data: mismatch between nrow(sc_keep) and length(build$pseudo_weights).",
    fixed = TRUE
  )
})
