data(sc)
data(sp1)
data(sp2)


# ── one-reference ────────────────────────────────────────────────────────────

test_that("p_formula_construction (one-ref): returns a formula", {
  res <- p_formula_construction(sc, sp1, weight = "sp_wts")

  expect_true(inherits(res$p_formula, "formula"))
})

test_that("p_formula_construction (one-ref): formula uses only shared variables", {
  res  <- p_formula_construction(sc, sp1, weight = "sp_wts")
  vars <- all.vars(res$p_formula)

  shared <- intersect(colnames(sc), colnames(sp1))
  expect_true(all(vars %in% shared))
})

test_that("p_formula_construction (one-ref): weight column excluded from formula", {
  res  <- p_formula_construction(sc, sp1, weight = "wts_sp1")
  vars <- all.vars(res$p_formula)

  expect_false("wts_sp1" %in% vars)
})

test_that("p_formula_construction (one-ref): returns log_messages as character", {
  res <- p_formula_construction(sc, sp1, weight = "sp_wts")

  expect_type(res$log_messages, "character")
  expect_gt(length(res$log_messages), 0)
})

test_that("p_formula_construction (one-ref): errors when no shared covariates", {
  sc_only <- data.frame(only_in_sc = 1:5)
  sp_only <- data.frame(sp_wts = rep(1, 5))

  expect_error(
    p_formula_construction(sc_only, sp_only, weight = "sp_wts"),
    "No shared covariates found to build default p_formula",
    fixed = TRUE
  )
})

test_that("p_formula_construction (one-ref): errors when only shared column is weight", {
  sc_w <- data.frame(wts_sp1 = 1:5, x = letters[1:5])
  sp_w <- data.frame(wts_sp1 = rep(1, 5))

  expect_error(
    p_formula_construction(sc_w, sp_w, weight = "wts_sp1"),
    "No shared covariates found to build default p_formula",
    fixed = TRUE
  )
})

test_that("p_formula_construction (one-ref): errors when sp is not a data.frame or list", {
  expect_error(
    p_formula_construction(sc, "not_a_df", weight = "sp_wts"),
    "`sp` must be either a data.frame or a list of data.frames.",
    fixed = TRUE
  )
})


# ── multi-reference ──────────────────────────────────────────────────────────

test_that("p_formula_construction (multi-ref): returns a list of formulas", {
  res <- p_formula_construction(
    sc, list(sp1, sp2),
    weight  = c("wts_sp1", "wts_sp2")
  )

  expect_true(is.list(res$p_formula))
  expect_length(res$p_formula, 2)
  expect_true(all(vapply(res$p_formula, inherits, logical(1), "formula")))
})

test_that("p_formula_construction (multi-ref): each formula excludes its own weight", {
  res <- p_formula_construction(
    sc, list(sp1, sp2),
    weight  = c("wts_sp1", "wts_sp2")
  )

  expect_false("wts_sp1" %in% all.vars(res$p_formula[[1]]))
  expect_false("wts_sp2" %in% all.vars(res$p_formula[[2]]))
})

test_that("p_formula_construction (multi-ref): list names assigned correctly", {
  sp_named <- list(survey_a = sp1, survey_b = sp2)

  res <- p_formula_construction(
    sc, sp_named,
    weight  = c("wts_sp1", "wts_sp2")
  )

  expect_equal(names(res$p_formula), c("survey_a", "survey_b"))
})

test_that("p_formula_construction (multi-ref): unnamed sp gets default names", {
  res <- p_formula_construction(
    sc, list(sp1, sp2),
    weight  = c("wts_sp1", "wts_sp2")
  )

  expect_equal(names(res$p_formula), c("sp[[1]]", "sp[[2]]"))
})

test_that("p_formula_construction (multi-ref): log_messages has one entry per sp", {
  res <- p_formula_construction(
    sc, list(sp1, sp2),
    weight  = c("wts_sp1", "wts_sp2")
  )

  expect_length(res$log_messages, 2)
})

test_that("p_formula_construction (multi-ref): errors when weight length mismatches sp", {
  expect_error(
    p_formula_construction(
      sc, list(sp1, sp2),
      weight  = "wts_sp1"
    ),
    "For multi-reference case, `weight` must be a character vector with same length as `sp`.",
    fixed = TRUE
  )
})

test_that("p_formula_construction (multi-ref): errors when no shared covariates for one sp", {
  sp_no_shared <- data.frame(wts_sp2 = rep(1, 5))

  expect_error(
    p_formula_construction(
      sc, list(sp1, sp_no_shared),
      weight  = c("wts_sp1", "wts_sp2")
    ),
    "No shared covariates found to build default p_formula",
    fixed = TRUE
  )
})
