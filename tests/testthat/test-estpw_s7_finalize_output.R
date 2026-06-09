# test-build_s7_finalize_output.R
# Tests for finalize_pw_fit()
#
# Function signature:
#   finalize_pw_fit(result, sc_out, sc0, sc_wname, na_mode, keep_sc, na_action_obj)
#
# The function:
#   - sets result$sc_updated  = sc_out
#   - sets result$pseudo_weights = sc_out[[sc_wname]]
#   - sets result$internal$raw_sc = sc0
#   - sets result$internal$na = list(na_mode, keep_sc, na_action_obj)
#   - assigns class "pw_fit"
#   - keeps any pre-existing fields in result

# setup ----

set.seed(2026)
n0 <- 10L

sc0 <- data.frame(
  id = seq_len(n0),
  x1 = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10),
  x2 = rnorm(n0)
)

keep_sc  <- !is.na(sc0$x1)        # 8 kept rows
excluded <- which(!keep_sc)
na_action_obj <- structure(excluded, class = "omit")
sc_wname <- "pseudo_wts"

# sc_out for "omit": 8 rows with weight column
set.seed(7)
w_fit_omit  <- runif(sum(keep_sc), 10, 100)
sc_out_omit <- sc0[keep_sc, ]
sc_out_omit[[sc_wname]] <- w_fit_omit

# sc_out for "exclude": all 10 rows, NA at excluded positions
w_full_excl <- rep(NA_real_, n0)
w_full_excl[keep_sc] <- runif(sum(keep_sc), 10, 100)
sc_out_excl <- sc0
sc_out_excl[[sc_wname]] <- w_full_excl

# sc_out for "fail" / "pass": all 10 rows, no NAs
w_fit_full  <- runif(n0, 10, 100)
sc_out_full <- sc0
sc_out_full[[sc_wname]] <- w_fit_full

# base result list representing earlier pipeline outputs
base_result <- list(
  method       = "calibration",
  coefficients = c("(Intercept)" = 0.5, x1 = 0.1),
  solver_diagnostics = list(termcd = 1L, fmax = 1e-10),
  internal     = list(Xc = matrix(1:6, 3, 2))
)


# class assignment ----

test_that("finalize_pw_fit: returned object has class 'pw_fit'", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_s3_class(out, "pw_fit")
})


# result$sc_updated ----

test_that("finalize_pw_fit: sc_updated equals sc_out passed in", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_identical(out$sc_updated, sc_out_omit)
})

test_that("finalize_pw_fit: sc_updated is a data frame", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_s3_class(out$sc_updated, "data.frame")
})


# result$pseudo_weights ----

test_that("finalize_pw_fit omit: pseudo_weights equals sc_out[[sc_wname]]", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(out$pseudo_weights, sc_out_omit[[sc_wname]])
})

test_that("finalize_pw_fit omit: pseudo_weights has length sum(keep_sc)", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_length(out$pseudo_weights, sum(keep_sc))
})

test_that("finalize_pw_fit omit: pseudo_weights has no NA", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_false(anyNA(out$pseudo_weights))
})

test_that("finalize_pw_fit exclude: pseudo_weights has length nrow(sc0)", {
  out <- finalize_pw_fit(base_result, sc_out_excl, sc0, sc_wname,
                         "exclude", keep_sc, na_action_obj)
  expect_length(out$pseudo_weights, n0)
})

test_that("finalize_pw_fit exclude: pseudo_weights is NA at excluded rows", {
  out <- finalize_pw_fit(base_result, sc_out_excl, sc0, sc_wname,
                         "exclude", keep_sc, na_action_obj)
  expect_true(all(is.na(out$pseudo_weights[excluded])))
})

test_that("finalize_pw_fit exclude: pseudo_weights non-NA count equals sum(keep_sc)", {
  out <- finalize_pw_fit(base_result, sc_out_excl, sc0, sc_wname,
                         "exclude", keep_sc, na_action_obj)
  expect_equal(sum(!is.na(out$pseudo_weights)), sum(keep_sc))
})

test_that("finalize_pw_fit fail: pseudo_weights has length nrow(sc0) and no NA", {
  out <- finalize_pw_fit(base_result, sc_out_full, sc0, sc_wname,
                         "fail", rep(TRUE, n0), NULL)
  expect_length(out$pseudo_weights, n0)
  expect_false(anyNA(out$pseudo_weights))
})

test_that("finalize_pw_fit pass: pseudo_weights has length nrow(sc0) and no NA", {
  out <- finalize_pw_fit(base_result, sc_out_full, sc0, sc_wname,
                         "pass", rep(TRUE, n0), NULL)
  expect_length(out$pseudo_weights, n0)
  expect_false(anyNA(out$pseudo_weights))
})


# result$internal$raw_sc ----

test_that("finalize_pw_fit: internal$raw_sc equals sc0", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_identical(out$internal$raw_sc, sc0)
})

test_that("finalize_pw_fit: internal$raw_sc is the original unmodified sc0", {
  # raw_sc should have all n0 rows regardless of na_mode
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(nrow(out$internal$raw_sc), n0)
})


# result$internal$na ----

test_that("finalize_pw_fit: internal$na contains na_mode, keep_sc, na_action_obj", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expected_names <- c("na_mode", "keep_sc", "na_action_obj")
  expect_true(all(expected_names %in% names(out$internal$na)))
})

test_that("finalize_pw_fit: internal$na$na_mode is stored correctly", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(out$internal$na$na_mode, "omit")
})

test_that("finalize_pw_fit: internal$na$keep_sc is stored correctly", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_identical(out$internal$na$keep_sc, keep_sc)
})

test_that("finalize_pw_fit: internal$na$na_action_obj is stored correctly", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_identical(out$internal$na$na_action_obj, na_action_obj)
})

test_that("finalize_pw_fit: internal$na$na_action_obj is NULL when passed NULL", {
  out <- finalize_pw_fit(base_result, sc_out_full, sc0, sc_wname,
                         "pass", rep(TRUE, n0), NULL)
  expect_null(out$internal$na$na_action_obj)
})

test_that("finalize_pw_fit: internal$na$na_mode = 'exclude' stored correctly", {
  out <- finalize_pw_fit(base_result, sc_out_excl, sc0, sc_wname,
                         "exclude", keep_sc, na_action_obj)
  expect_equal(out$internal$na$na_mode, "exclude")
})


# pre-existing fields kept ----

test_that("finalize_pw_fit: pre-existing method field is kept", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(out$method, base_result$method)
})

test_that("finalize_pw_fit: pre-existing coefficients field is kept", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(out$coefficients, base_result$coefficients)
})

test_that("finalize_pw_fit: pre-existing solver_diagnostics field is kept", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_equal(out$solver_diagnostics, base_result$solver_diagnostics)
})

test_that("finalize_pw_fit: pre-existing internal$Xc is kept after adding raw_sc and na", {
  out <- finalize_pw_fit(base_result, sc_out_omit, sc0, sc_wname,
                         "omit", keep_sc, na_action_obj)
  expect_identical(out$internal$Xc, base_result$internal$Xc)
})
