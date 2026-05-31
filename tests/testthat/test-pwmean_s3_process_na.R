# Setup ----

data(sc)

# helper: build a minimal sc_data list as prepare_sc_data() would return
make_sc_data <- function(sc_df, idx = NULL) {
  if (is.null(idx)) idx <- seq_len(nrow(sc_df))
  n <- length(idx)
  list(
    sc       = sc_df[idx, , drop = FALSE],
    X        = matrix(1.0, nrow = n, ncol = 1L),
    w        = rep(1.0, n),
    idx_keep = idx
  )
}

sc_data <- make_sc_data(sc)


# clean path ----

test_that("process_na_yz: clean path returns correct list names", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_named(res, c("Y", "X", "w", "sc", "y_name", "zcol", "domain", "na_info"))
})

test_that("process_na_yz: Y is a numeric vector with no missing values", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_type(res$Y, "double")
  expect_false(anyNA(res$Y))
})

test_that("process_na_yz: na_info contains correct fields", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_named(res$na_info, c("na_action", "n_omitted", "n_used", "omitted_raw", "kept_raw"))
})

test_that("process_na_yz: n_omitted + n_used equals nrow of input sc", {
  sc_na              <- sc
  sc_na$psa_level[1:5] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  res                <- process_na_yz(sc_data_na, y = "psa_level")
  expect_equal(res$na_info$n_omitted + res$na_info$n_used, nrow(sc_na))
})

test_that("process_na_yz: no NAs in y gives n_omitted = 0 and n_used = nrow(sc)", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_equal(res$na_info$n_omitted, 0L)
  expect_equal(res$na_info$n_used, nrow(sc))
})


# all-rows-missing guard ----

test_that("process_na_yz: errors when all y values are missing", {
  sc_all_na              <- sc
  sc_all_na$psa_level    <- NA_real_
  sc_data_all_na         <- make_sc_data(sc_all_na)
  expect_error(
    process_na_yz(sc_data_all_na, y = "psa_level"),
    "No complete cases remain after removing missing values in 'y' and 'zcol'.",
    fixed = TRUE
  )
})

test_that("process_na_yz: errors when all zcol values are missing", {
  sc_all_na         <- sc
  sc_all_na$grp_na  <- rep(NA_character_, nrow(sc))
  sc_data_all_na    <- make_sc_data(sc_all_na)
  expect_error(
    process_na_yz(sc_data_all_na, y = "psa_level", zcol = "grp_na"),
    "No complete cases remain after removing missing values in 'y' and 'zcol'.",
    fixed = TRUE
  )
})


# na.action checks ----

test_that("process_na_yz: na.fail errors when all y values are missing", {
  sc_na           <- sc
  sc_na$psa_level <- NA_real_
  sc_data_na      <- make_sc_data(sc_na)
  expect_error(
    process_na_yz(sc_data_na, y = "psa_level", na.action = stats::na.fail),
    "Missing values detected in `y` or `zcol`.",
    fixed = TRUE
  )
})

test_that("process_na_yz: na.pass is rejected even when all values are missing", {
  sc_na           <- sc
  sc_na$psa_level <- NA_real_
  sc_data_na      <- make_sc_data(sc_na)
  expect_error(
    process_na_yz(sc_data_na, y = "psa_level", na.action = stats::na.pass),
    "`na.pass` is not supported at estimate stage.",
    fixed = TRUE
  )
})

test_that("process_na_yz: na.fail errors when y has missing values", {
  sc_na              <- sc
  sc_na$psa_level[1] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  expect_error(
    process_na_yz(sc_data_na, y = "psa_level", na.action = stats::na.fail),
    "Missing values detected in `y` or `zcol`.",
    fixed = TRUE
  )
})

test_that("process_na_yz: na.fail passes when no missing values present", {
  res <- process_na_yz(sc_data, y = "psa_level", na.action = stats::na.fail)
  expect_equal(res$na_info$n_omitted, 0L)
})

test_that("process_na_yz: na.pass is rejected", {
  expect_error(
    process_na_yz(sc_data, y = "psa_level", na.action = stats::na.pass),
    "`na.pass` is not supported at estimate stage.",
    fixed = TRUE
  )
})

test_that("process_na_yz: unsupported custom na.action function is rejected", {
  expect_error(
    process_na_yz(sc_data, y = "psa_level", na.action = mean),
    "Unsupported na.action function",
    fixed = TRUE
  )
})


# na_obj class and na_info counts ----

test_that("process_na_yz: na.omit gives na_action class 'omit'", {
  sc_na              <- sc
  sc_na$psa_level[1:3] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  res                <- process_na_yz(sc_data_na, y = "psa_level",
                                      na.action = stats::na.omit)
  expect_equal(class(res$na_info$na_action), "omit")
})

test_that("process_na_yz: na.exclude gives na_action class 'exclude'", {
  sc_na              <- sc
  sc_na$psa_level[1:3] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  res                <- process_na_yz(sc_data_na, y = "psa_level",
                                      na.action = stats::na.exclude)
  expect_equal(class(res$na_info$na_action), "exclude")
})

test_that("process_na_yz: omitted_raw contains original indices of missing rows", {
  sc_na                    <- sc
  sc_na$psa_level[c(2, 4)] <- NA_real_
  sc_data_na               <- make_sc_data(sc_na)
  res                      <- process_na_yz(sc_data_na, y = "psa_level")
  expect_equal(as.integer(res$na_info$omitted_raw), c(2L, 4L))
})

test_that("process_na_yz: kept_raw and omitted_raw partition idx_keep", {
  sc_na              <- sc
  sc_na$psa_level[1:5] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  res                <- process_na_yz(sc_data_na, y = "psa_level")
  all_idx            <- sort(c(res$na_info$kept_raw, res$na_info$omitted_raw))
  expect_equal(all_idx, sc_data_na$idx_keep)
})

test_that("process_na_yz: kept_raw and omitted_raw are disjoint", {
  sc_na              <- sc
  sc_na$psa_level[1:5] <- NA_real_
  sc_data_na         <- make_sc_data(sc_na)
  res                <- process_na_yz(sc_data_na, y = "psa_level")
  expect_length(intersect(res$na_info$kept_raw, res$na_info$omitted_raw), 0L)
})


# output subsetting alignment ----

test_that("process_na_yz: Y, X, w, sc all have row count equal to n_used", {
  sc_na                <- sc
  sc_na$psa_level[1:5] <- NA_real_
  sc_data_na           <- make_sc_data(sc_na)
  res                  <- process_na_yz(sc_data_na, y = "psa_level")
  n_used               <- res$na_info$n_used
  expect_length(res$Y, n_used)
  expect_equal(nrow(res$X),  n_used)
  expect_length(res$w,  n_used)
  expect_equal(nrow(res$sc), n_used)
})

test_that("process_na_yz: Y values equal the non-missing y entries in original order", {
  sc_na                  <- sc
  sc_na$psa_level[c(2, 4)] <- NA_real_
  sc_data_na             <- make_sc_data(sc_na)
  res                    <- process_na_yz(sc_data_na, y = "psa_level")
  expect_equal(res$Y, sc_na$psa_level[!is.na(sc_na$psa_level)])
})


# pass-through fields ----

test_that("process_na_yz: y_name and zcol are stored in the return list", {
  res <- process_na_yz(sc_data, y = "psa_level", zcol = "race")
  expect_equal(res$y_name, "psa_level")
  expect_equal(res$zcol,   "race")
})

test_that("process_na_yz: zcol is NULL in the return list when zcol = NULL", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_null(res$zcol)
})


# domain integration with standardize_zcol ----

test_that("process_na_yz: zcol = NULL gives domain$mode = 'overall'", {
  res <- process_na_yz(sc_data, y = "psa_level")
  expect_equal(res$domain$mode, "overall")
})

test_that("process_na_yz: factor zcol gives domain$mode = 'factor' with matching labels", {
  res <- process_na_yz(sc_data, y = "psa_level", zcol = "race")
  expect_equal(res$domain$mode, "factor")
  expect_equal(sort(res$domain$labels), sort(levels(droplevels(factor(sc$race)))))
})

test_that("process_na_yz: binary numeric zcol gives domain$mode = 'binary'", {
  sc_bin              <- sc
  sc_bin$flag         <- rep(c(0L, 1L), length.out = nrow(sc))
  sc_data_bin         <- make_sc_data(sc_bin)
  res                 <- process_na_yz(sc_data_bin, y = "psa_level", zcol = "flag")
  expect_equal(res$domain$mode, "binary")
})


# na_obj when no NAs are present ----

test_that("process_na_yz: na.omit with no NAs gives na_action = NULL", {
  res <- process_na_yz(sc_data, y = "psa_level", na.action = stats::na.omit)
  expect_null(res$na_info$na_action)
})

test_that("process_na_yz: na.exclude with no NAs gives na_action = NULL", {
  res <- process_na_yz(sc_data, y = "psa_level", na.action = stats::na.exclude)
  expect_null(res$na_info$na_action)
})
