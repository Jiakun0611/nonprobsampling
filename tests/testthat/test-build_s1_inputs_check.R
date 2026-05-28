data(sc)
data(sp1)
data(sp2)

options(survey.lonely.psu = "adjust")

des1 <- survey::svydesign(
  ids = ~psu_sp1,
  strata = ~strata_sp1,
  weights = ~wts_sp1,
  data = sp1,
  nest = TRUE
)

des2 <- survey::svydesign(
  ids = ~psu_sp2,
  strata = ~strata_sp2,
  weights = ~wts_sp2,
  data = sp2,
  nest = TRUE
)

test_that("check_ipwm_inputs_build validates correct one-reference input", {
  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "alp"
    )
  )

  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "clw"
    )
  )

  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "calibration"
    )
  )

  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "cali"
    )
  )
})


test_that("check_ipwm_inputs_build validates correct multi-reference input", {
  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "size",
      precali = TRUE
    )
  )

  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "given",
      precali = FALSE
    )
  )
})


test_that("check_ipwm_inputs_build rejects invalid data structure", {
  expect_error(
    check_ipwm_inputs_build(
      data = 42L,
      p_formula = NULL,
      method = "alp"
    ),
    "`data` must be a list",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc),
      p_formula = NULL,
      method = "alp"
    ),
    "`data` must be a list",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(as.matrix(sc), sc),
      p_formula = NULL,
      method = "alp"
    ),
    "The first element of `data` must be a data.frame",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects invalid survey design objects", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, sp1),
      p_formula = NULL,
      method = "alp"
    ),
    "not valid survey design objects",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects zero-row sc", {
  sc_empty <- sc[0, ]

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc_empty, des1),
      p_formula = NULL,
      method = "alp"
    ),
    "`sc` has zero rows.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects duplicated sc column names", {
  sc_dup <- sc
  names(sc_dup)[2] <- names(sc_dup)[1]

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc_dup, des1),
      p_formula = NULL,
      method = "alp"
    ),
    "`sc` has duplicated column names.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects invalid method", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "bad_method"
    ),
    "Invalid method",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = c("alp", "clw")
    ),
    "`method` must be a single character string or NULL.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build checks method-reference consistency", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = "multi"
    ),
    "`method = 'multi'` is only allowed when multiple reference samples are provided.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "alp",
      sp_order = "size",
      precali = TRUE
    ),
    "only valid for a single reference sample",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build checks p_formula in one-reference case", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = list(~ agecat),
      method = "alp"
    ),
    "For one-reference case, `p_formula` must be a formula or NULL.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = ~ not_a_variable,
      method = "alp"
    ),
    "Variables in `p_formula` not found in `sc`",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = agecat ~ marital,
      method = "alp"
    ),
    "`p_formula` must be a one-sided formula (e.g., ~ x + y), not two-sided.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects two-sided formula in multi-reference case", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = list(~ agecat, agecat ~ marital),
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "All formulas in `p_formula` must be one-sided (e.g., ~ x + y), not two-sided.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build checks sp_order and precali in multi-reference case", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      precali = TRUE
    ),
    "`sp_order` must be one of 'size' or 'given'.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "wrong",
      precali = TRUE
    ),
    "`sp_order` must be one of 'size' or 'given'.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "size"
    ),
    "`precali` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "size",
      precali = "TRUE"
    ),
    "`precali` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build checks p_formula in multi-reference case", {
  p_formula <- ~ agecat + marital + race + educat + empstat +
    smoking + comorbidity

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = p_formula,
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "For multi-reference case, `p_formula` must be a list",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = list(p_formula),
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "For multi-reference case, `p_formula` must be a list of 2 formulas.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = list(p_formula, "not_formula"),
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "All elements of `p_formula` must be formulas.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = list(~ agecat, ~ not_a_variable),
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "Variables in `p_formula[[2]]` not found in `sc`",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build accepts NULL method and NULL p_formula", {
  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = NULL
    )
  )

  expect_invisible(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = NULL,
      sp_order = "size",
      precali = TRUE
    )
  )
})


test_that("check_ipwm_inputs_build rejects method = NA", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1),
      p_formula = NULL,
      method = NA_character_
    ),
    "`method` must be a single character string or NULL.",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects p_formula variables missing from sp", {
  sc_extra <- sc
  sc_extra$only_in_sc <- 1L

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc_extra, des1),
      p_formula = ~ only_in_sc,
      method = "alp"
    ),
    "Variables in `p_formula` not found in corresponding `sp`",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc_extra, des1, des2),
      p_formula = list(~ agecat, ~ only_in_sc),
      method = "multi",
      sp_order = "size",
      precali = TRUE
    ),
    "Variables in `p_formula[[2]]` not found in corresponding `sp`",
    fixed = TRUE
  )
})


test_that("check_ipwm_inputs_build rejects invalid precali values", {
  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "size",
      precali = NA
    ),
    "`precali` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )

  expect_error(
    check_ipwm_inputs_build(
      data = list(sc, des1, des2),
      p_formula = NULL,
      method = "multi",
      sp_order = "size",
      precali = c(TRUE, FALSE)
    ),
    "`precali` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})


# ── extract_analysis_data ────────────────────────────────────────────────────

test_that("extract_analysis_data: returns a data.frame", {
  out <- extract_analysis_data(des1)

  expect_s3_class(out, "data.frame")
})

test_that("extract_analysis_data: weight column 'sp_wts' is added", {
  out <- extract_analysis_data(des1)

  expect_true("sp_wts" %in% names(out))
})

test_that("extract_analysis_data: all weights are positive finite numbers", {
  out <- extract_analysis_data(des1)

  expect_true(all(is.finite(out$sp_wts)))
  expect_true(all(out$sp_wts > 0))
})

test_that("extract_analysis_data: design variables (psu_sp1, strata_sp1, wts_sp1) are removed", {
  out <- extract_analysis_data(des1)

  expect_false("psu_sp1"     %in% names(out))
  expect_false("strata_sp1"  %in% names(out))
  expect_false("wts_sp1" %in% names(out))
})

test_that("extract_analysis_data: row count matches original data", {
  out <- extract_analysis_data(des1)

  expect_equal(nrow(out), nrow(sp1))
})

test_that("extract_analysis_data: custom weight_name is used", {
  out <- extract_analysis_data(des1, weight_name = "my_wts")

  expect_true("my_wts"  %in% names(out))
  expect_false("sp_wts" %in% names(out))
})

test_that("extract_analysis_data: errors when des is not a survey design object", {
  expect_error(
    extract_analysis_data(sp1),
    "`des` must be a survey design object.",
    fixed = TRUE
  )
})

test_that("extract_analysis_data: errors when weight_name already exists in analysis vars", {
  # Build a design where a covariate is named "sp_wts"
  df_clash <- sp1
  df_clash$sp_wts <- 1L
  des_clash <- survey::svydesign(
    ids     = ~psu_sp1,
    strata  = ~strata_sp1,
    weights = ~wts_sp1,
    data    = df_clash,
    nest    = TRUE
  )

  expect_error(
    extract_analysis_data(des_clash, weight_name = "sp_wts"),
    "already exists",
    fixed = TRUE
  )
})


# ── parse_ipwm_data ──────────────────────────────────────────────────────────

test_that("parse_ipwm_data: returns list with sc, sp_des, sp_vars, n_ref", {
  out <- parse_ipwm_data(list(sc, des1))

  expect_named(out, c("sc", "sp_des", "sp_vars", "n_ref"))
})

test_that("parse_ipwm_data: sc is the first element unchanged", {
  out <- parse_ipwm_data(list(sc, des1))

  expect_identical(out$sc, sc)
})

test_that("parse_ipwm_data: n_ref is 1 for one reference survey", {
  out <- parse_ipwm_data(list(sc, des1))

  expect_equal(out$n_ref, 1L)
})

test_that("parse_ipwm_data: n_ref is 2 for two reference surveys", {
  out <- parse_ipwm_data(list(sc, des1, des2))

  expect_equal(out$n_ref, 2L)
})

test_that("parse_ipwm_data: sp_des is a named list of design objects", {
  out <- parse_ipwm_data(list(sc, des1, des2))

  expect_true(is.list(out$sp_des))
  expect_length(out$sp_des, 2L)
  expect_true(all(vapply(out$sp_des, inherits, logical(1), "survey.design2")))
})

test_that("parse_ipwm_data: sp_vars are plain data.frames with 'sp_wts' column", {
  out <- parse_ipwm_data(list(sc, des1))

  expect_s3_class(out$sp_vars[[1]], "data.frame")
  expect_true("sp_wts" %in% names(out$sp_vars[[1]]))
})

test_that("parse_ipwm_data: unnamed list gets default names sp[[1]], sp[[2]]", {
  out <- parse_ipwm_data(list(sc, des1, des2))

  expect_equal(names(out$sp_des),  c("sp[[1]]", "sp[[2]]"))
  expect_equal(names(out$sp_vars), c("sp[[1]]", "sp[[2]]"))
})

test_that("parse_ipwm_data: named list preserves names", {
  out <- parse_ipwm_data(list(sc, survey_a = des1, survey_b = des2))

  expect_equal(names(out$sp_des),  c("survey_a", "survey_b"))
  expect_equal(names(out$sp_vars), c("survey_a", "survey_b"))
})

test_that("parse_ipwm_data: partially named list falls back to default names", {
  out <- parse_ipwm_data(list(sc, survey_a = des1, des2))

  expect_equal(names(out$sp_des),  c("sp[[1]]", "sp[[2]]"))
  expect_equal(names(out$sp_vars), c("sp[[1]]", "sp[[2]]"))
})
