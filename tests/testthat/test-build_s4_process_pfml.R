data(sc)
data(sp1)
data(sp2)


# ── one-reference: return structure ─────────────────────────────────────────

test_that("process_p_formula (one-ref): returns sc, sp, vars, log_messages", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_named(res, c("sc", "sp", "vars", "log_messages"))
  expect_s3_class(res$sc, "data.frame")
  expect_s3_class(res$sp, "data.frame")
  expect_type(res$vars, "character")
  expect_type(res$log_messages, "character")
})

test_that("process_p_formula (one-ref): sc and sp have same column names", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_equal(setdiff(colnames(res$sc), colnames(res$sp)), character(0))
})

test_that("process_p_formula (one-ref): intercept not in output columns", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_false("(Intercept)" %in% colnames(res$sc))
  expect_false("(Intercept)" %in% colnames(res$sp))
})

test_that("process_p_formula (one-ref): weight column attached to sp only", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_true("wts_sp1" %in% colnames(res$sp))
  expect_false("wts_sp1" %in% colnames(res$sc))
})

test_that("process_p_formula (one-ref): sc row count unchanged", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_equal(nrow(res$sc), nrow(sc))
})

test_that("process_p_formula (one-ref): sp row count unchanged", {
  res <- process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~agecat)

  expect_equal(nrow(res$sp), nrow(sp1))
})


# ── one-reference: input validation ─────────────────────────────────────────

test_that("process_p_formula (one-ref): errors when p_formula is not a formula", {
  expect_error(
    process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = "agecat"),
    "'p_formula' must be a formula for one-reference case.",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): errors when variable missing from sc", {
  expect_error(
    process_p_formula(sc, sp1, weight = "wts_sp1", p_formula = ~only_in_sp),
    "Missing variable(s)",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): errors when variable missing from sp", {
  sc_extra <- sc
  sc_extra$only_in_sc <- 1L

  expect_error(
    process_p_formula(sc_extra, sp1, weight = "wts_sp1", p_formula = ~only_in_sc),
    "Missing variable(s)",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): errors when weight column not in sp", {
  expect_error(
    process_p_formula(sc, sp1, weight = "nonexistent", p_formula = ~agecat),
    "Weight column not found in sp.",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): errors when sp is not data.frame or list", {
  expect_error(
    process_p_formula(sc, "not_a_df", weight = "wts_sp1", p_formula = ~agecat),
    "sp must be a data.frame or a list of data.frames.",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): errors when factor levels differ between sc and sp", {
  sp_bad <- sp1
  sp_bad$agecat <- factor(sp_bad$agecat, levels = levels(sp_bad$agecat)[-1])

  expect_error(
    process_p_formula(sc, sp_bad, weight = "wts_sp1", p_formula = ~agecat),
    "Factor levels of 'agecat' differ between sc and sp",
    fixed = TRUE
  )
})

test_that("process_p_formula (one-ref): factor level error message names the variable", {
  sp_bad <- sp1
  sp_bad$agecat <- factor(sp_bad$agecat, levels = levels(sp_bad$agecat)[-1])

  err <- tryCatch(
    process_p_formula(sc, sp_bad, weight = "wts_sp1", p_formula = ~agecat),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "agecat")
  expect_match(err, "sc :")
  expect_match(err, "sp :")
})


# ── multi-reference: return structure ────────────────────────────────────────

test_that("process_p_formula (multi-ref): returns sc, sp, vars, log_messages", {
  res <- process_p_formula(
    sc, list(sp1, sp2),
    weight        = c("wts_sp1", "wts_sp2"),
    p_formula     = list(~agecat, ~agecat),
    Pre.calibration = FALSE
  )

  expect_named(res, c("sc", "sp", "vars", "log_messages"))
  expect_s3_class(res$sc, "data.frame")
  expect_true(is.list(res$sp))
  expect_length(res$sp, 2)
  expect_true(is.list(res$vars))
  expect_length(res$vars, 2)
})

test_that("process_p_formula (multi-ref): each sp_new[[j]] has its weight column", {
  res <- process_p_formula(
    sc, list(sp1, sp2),
    weight        = c("wts_sp1", "wts_sp2"),
    p_formula     = list(~agecat, ~agecat),
    Pre.calibration = FALSE
  )

  expect_true("wts_sp1" %in% colnames(res$sp[[1]]))
  expect_true("wts_sp2" %in% colnames(res$sp[[2]]))
})

test_that("process_p_formula (multi-ref): sc_new deduplicates shared columns", {
  res <- process_p_formula(
    sc, list(sp1, sp2),
    weight        = c("wts_sp1", "wts_sp2"),
    p_formula     = list(~agecat, ~agecat),
    Pre.calibration = FALSE
  )

  agecat_cols <- grep("^agecat", colnames(res$sc), value = TRUE)
  expect_equal(length(agecat_cols), length(unique(agecat_cols)))
})

test_that("process_p_formula (multi-ref): sp names preserved from named list", {
  res <- process_p_formula(
    sc, list(survey_a = sp1, survey_b = sp2),
    weight        = c("wts_sp1", "wts_sp2"),
    p_formula     = list(~agecat, ~agecat),
    Pre.calibration = FALSE
  )

  expect_equal(names(res$sp), c("survey_a", "survey_b"))
})

test_that("process_p_formula (multi-ref): Pre.calibration = FALSE adds log message", {
  res <- process_p_formula(
    sc, list(sp1, sp2),
    weight        = c("wts_sp1", "wts_sp2"),
    p_formula     = list(~agecat, ~agecat),
    Pre.calibration = FALSE
  )

  expect_true(any(grepl("Pre-calibration is recommended", res$log_messages)))
})


# ── multi-reference: input validation ───────────────────────────────────────

test_that("process_p_formula (multi-ref): errors when p_formula is not a list of formulas", {
  expect_error(
    process_p_formula(
      sc, list(sp1, sp2),
      weight        = c("wts_sp1", "wts_sp2"),
      p_formula     = ~agecat,
      Pre.calibration = FALSE
    ),
    "For multi-reference, 'p_formula' must be a list of formulas.",
    fixed = TRUE
  )
})

test_that("process_p_formula (multi-ref): errors when p_formula length mismatches sp", {
  expect_error(
    process_p_formula(
      sc, list(sp1, sp2),
      weight        = c("wts_sp1", "wts_sp2"),
      p_formula     = list(~agecat),
      Pre.calibration = FALSE
    ),
    "For multi-reference, 'p_formula' must have the same length as 'sp'.",
    fixed = TRUE
  )
})

test_that("process_p_formula (multi-ref): errors when weight length mismatches sp", {
  expect_error(
    process_p_formula(
      sc, list(sp1, sp2),
      weight        = "wts_sp1",
      p_formula     = list(~agecat, ~agecat),
      Pre.calibration = FALSE
    ),
    "For multi-reference, 'weight' must be a character vector with length = length(sp).",
    fixed = TRUE
  )
})

test_that("process_p_formula (multi-ref): errors when weight column missing from sp[[j]]", {
  expect_error(
    process_p_formula(
      sc, list(sp1, sp2),
      weight        = c("wts_sp1", "nonexistent"),
      p_formula     = list(~agecat, ~agecat),
      Pre.calibration = FALSE
    ),
    "Weight column 'nonexistent' not found in sp[2].",
    fixed = TRUE
  )
})

test_that("process_p_formula (multi-ref): errors when variable missing from sp[[j]]", {
  sc_extra <- sc
  sc_extra$only_in_sc <- 1L

  expect_error(
    process_p_formula(
      sc_extra, list(sp1, sp2),
      weight        = c("wts_sp1", "wts_sp2"),
      p_formula     = list(~agecat, ~only_in_sc),
      Pre.calibration = FALSE
    ),
    "For reference 2, missing vars",
    fixed = TRUE
  )
})

test_that("process_p_formula (multi-ref): errors when factor levels differ in sp[[j]]", {
  sp2_bad <- sp2
  sp2_bad$agecat <- factor(sp2_bad$agecat, levels = levels(sp2_bad$agecat)[-1])

  expect_error(
    process_p_formula(
      sc, list(sp1, sp2_bad),
      weight        = c("wts_sp1", "wts_sp2"),
      p_formula     = list(~agecat, ~agecat),
      Pre.calibration = FALSE
    ),
    "Factor levels of 'agecat' differ between sc and sp[[2]]",
    fixed = TRUE
  )
})
