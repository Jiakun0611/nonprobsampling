# test-build_s6_sc_update.R
# Tests for reconstruct_sc_output()
#
# Function signature:
#   reconstruct_sc_output(sc0, w_fit, keep_sc, na_mode, na_action_obj, sc_wname)
#
# Four na_mode branches are tested separately:
#   "omit"    -- only kept rows returned; no NA in weight column
#   "exclude" -- all rows returned; NA weight for dropped rows; na.action attr set
#   "fail"    -- all rows returned; w_fit must equal nrow(sc0)
#   "pass"    -- same as "fail"

# setup ----

set.seed(2026)
n0 <- 10L

# sc0 has two NA rows (rows 3 and 7) in x1
sc0 <- data.frame(
  id = seq_len(n0),
  x1 = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10),
  x2 = rnorm(n0)
)

keep_sc  <- !is.na(sc0$x1)           # TRUE for rows 1,2,4,5,6,8,9,10  (n_keep = 8)
n_keep   <- sum(keep_sc)             # 8
excluded <- which(!keep_sc)          # c(3, 7)

set.seed(7)
w_fit_kept <- runif(n_keep, 10, 100)  # weights for the 8 kept rows

# na_action_obj: structure produced by na.omit / na.exclude
na_action_obj <- structure(excluded, class = "omit")

sc_wname <- "pseudo_wts"

# Full-length weights for "fail" / "pass" tests (all rows kept)
keep_all   <- rep(TRUE, n0)
w_fit_full <- runif(n0, 10, 100)


# na_mode = "omit" ----

test_that("reconstruct_sc_output omit: result has sum(keep_sc) rows", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "omit", na_action_obj, sc_wname)
  expect_equal(nrow(out), n_keep)
})

test_that("reconstruct_sc_output omit: left rows match sc0[keep_sc, ]", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "omit", na_action_obj, sc_wname)
  expect_equal(out$id, sc0$id[keep_sc])
  expect_equal(out$x1, sc0$x1[keep_sc])
})

test_that("reconstruct_sc_output omit: weight column equals w_fit", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "omit", na_action_obj, sc_wname)
  expect_equal(out[[sc_wname]], w_fit_kept)
})

test_that("reconstruct_sc_output omit: weight column has no NA", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "omit", na_action_obj, sc_wname)
  expect_false(anyNA(out[[sc_wname]]))
})

test_that("reconstruct_sc_output omit: weight column is named sc_wname", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "omit", na_action_obj, sc_wname)
  expect_true(sc_wname %in% names(out))
})


# na_mode = "exclude" ----

test_that("reconstruct_sc_output exclude: result has nrow(sc0) rows", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_equal(nrow(out), n0)
})

test_that("reconstruct_sc_output exclude: kept rows have correct weights", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_equal(out[[sc_wname]][keep_sc], w_fit_kept)
})

test_that("reconstruct_sc_output exclude: excluded rows have NA weight", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_true(all(is.na(out[[sc_wname]][excluded])))
})

test_that("reconstruct_sc_output exclude: exactly n_keep non-NA weights", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_equal(sum(!is.na(out[[sc_wname]])), n_keep)
})

test_that("reconstruct_sc_output exclude: na.action attribute is set when na_action_obj is not NULL", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_false(is.null(attr(out, "na.action")))
  expect_equal(attr(out, "na.action"), na_action_obj)
})

test_that("reconstruct_sc_output exclude: no na.action attribute when na_action_obj is NULL", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", NULL, sc_wname)
  expect_null(attr(out, "na.action"))
})

test_that("reconstruct_sc_output exclude: original columns are kept unchanged", {
  out <- reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "exclude", na_action_obj, sc_wname)
  expect_equal(out$id, sc0$id)
  expect_equal(out$x1, sc0$x1)
})


# na_mode = "fail" ----

test_that("reconstruct_sc_output fail: result has nrow(sc0) rows", {
  out <- reconstruct_sc_output(sc0, w_fit_full, keep_all, "fail", NULL, sc_wname)
  expect_equal(nrow(out), n0)
})

test_that("reconstruct_sc_output fail: weight column equals w_fit", {
  out <- reconstruct_sc_output(sc0, w_fit_full, keep_all, "fail", NULL, sc_wname)
  expect_equal(out[[sc_wname]], w_fit_full)
})

test_that("reconstruct_sc_output fail: no NA in weight column", {
  out <- reconstruct_sc_output(sc0, w_fit_full, keep_all, "fail", NULL, sc_wname)
  expect_false(anyNA(out[[sc_wname]]))
})

test_that("reconstruct_sc_output fail: error when length(w_fit) != nrow(sc0)", {
  # The general length check passes because w_fit_kept and keep_sc
  # both correspond to the 8 left observations. For na_mode = "fail",
  # however, the fit must cover all 10 original observations, so the
  # fail-specific check should error.
  expect_error(
    reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "fail", NULL, sc_wname),
    "expected w_fit length",
    fixed = TRUE
  )
})


# na_mode = "pass" ----

test_that("reconstruct_sc_output pass: result has nrow(sc0) rows", {
  out <- reconstruct_sc_output(sc0, w_fit_full, keep_all, "pass", NULL, sc_wname)
  expect_equal(nrow(out), n0)
})

test_that("reconstruct_sc_output pass: weight column equals w_fit", {
  out <- reconstruct_sc_output(sc0, w_fit_full, keep_all, "pass", NULL, sc_wname)
  expect_equal(out[[sc_wname]], w_fit_full)
})

test_that("reconstruct_sc_output pass: error when length(w_fit) != nrow(sc0)", {
  # Same logic as the "fail" test above: n_fit == n_keep passes the first
  # check, but n_fit != n0 triggers the pass-specific error.
  expect_error(
    reconstruct_sc_output(sc0, w_fit_kept, keep_sc, "pass", NULL, sc_wname),
    "expected w_fit length",
    fixed = TRUE
  )
})


# length mismatch ----

test_that("reconstruct_sc_output: error when length(w_fit) != sum(keep_sc)", {
  w_wrong <- w_fit_kept[-1]   # one element too short
  expect_error(
    reconstruct_sc_output(sc0, w_wrong, keep_sc, "omit", na_action_obj, sc_wname),
    "Length mismatch",
    fixed = TRUE
  )
})

test_that("reconstruct_sc_output: error message reports actual counts", {
  w_wrong <- w_fit_kept[-1]
  expect_error(
    reconstruct_sc_output(sc0, w_wrong, keep_sc, "omit", na_action_obj, sc_wname),
    as.character(n_keep - 1L),   # reported length of w_fit
    fixed = TRUE
  )
})
