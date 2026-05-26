data(sc)
data(sp1)
data(sp2)

options(survey.lonely.psu = "adjust")

des1 <- survey::svydesign(
  ids     = ~psu_sp1,
  strata  = ~strata_sp1,
  weights = ~wts_sp1,
  data    = sp1,
  nest    = TRUE
)

des2 <- survey::svydesign(
  ids     = ~psu_sp2,
  strata  = ~strata_sp2,
  weights = ~wts_sp2,
  data    = sp2,
  nest    = TRUE
)


# ── resolve_na_action ────────────────────────────────────────────────────────

test_that("resolve_na_action handles function inputs", {
  expect_equal(resolve_na_action(stats::na.omit),    "omit")
  expect_equal(resolve_na_action(stats::na.exclude), "exclude")
  expect_equal(resolve_na_action(stats::na.fail),    "fail")
  expect_equal(resolve_na_action(stats::na.pass),    "pass")
})

test_that("resolve_na_action handles character inputs", {
  expect_equal(resolve_na_action("omit"),       "omit")
  expect_equal(resolve_na_action("na.omit"),    "omit")
  expect_equal(resolve_na_action("exclude"),    "exclude")
  expect_equal(resolve_na_action("na.exclude"), "exclude")
  expect_equal(resolve_na_action("fail"),       "fail")
  expect_equal(resolve_na_action("na.fail"),    "fail")
  expect_equal(resolve_na_action("pass"),       "pass")
  expect_equal(resolve_na_action("na.pass"),    "pass")
})

test_that("resolve_na_action rejects unsupported function", {
  expect_error(
    resolve_na_action(mean),
    "Unsupported na.action function",
    fixed = TRUE
  )
})

test_that("resolve_na_action rejects invalid string", {
  expect_error(
    resolve_na_action("drop"),
    "Invalid na.action string",
    fixed = TRUE
  )
})

test_that("resolve_na_action rejects invalid type", {
  expect_error(
    resolve_na_action(1L),
    "Invalid na.action type",
    fixed = TRUE
  )
})


# ── handle_na_for_ipwm ───────────────────────────────────────────────────────

test_that("handle_na_for_ipwm (omit): removes NA rows from sc and sp", {
  sc_na       <- sc;  sc_na$agecat[1:3] <- NA
  sp_na       <- sp1; sp_na$agecat[1:2] <- NA

  res <- handle_na_for_ipwm(sc_na, sp_na, ~agecat, na_mode = "omit")

  expect_equal(nrow(res$sc), nrow(sc) - 3)
  expect_equal(nrow(res$sp), nrow(sp1) - 2)
  expect_equal(sum(!res$keep_sc), 3)
  expect_equal(sum(!res$keep_sp), 2)
})

test_that("handle_na_for_ipwm (omit): na_action has class 'omit'", {
  sc_na <- sc; sc_na$agecat[1:2] <- NA

  res <- handle_na_for_ipwm(sc_na, sp1, ~agecat, na_mode = "omit")

  expect_equal(class(res$na_action), "omit")
  expect_equal(as.integer(res$na_action), 1:2)
})

test_that("handle_na_for_ipwm (exclude): na_action has class 'exclude'", {
  sc_na <- sc; sc_na$agecat[1] <- NA

  res <- handle_na_for_ipwm(sc_na, sp1, ~agecat, na_mode = "exclude")

  expect_equal(class(res$na_action), "exclude")
  expect_equal(as.integer(res$na_action), 1L)
})

test_that("handle_na_for_ipwm (fail): errors when NA in sc", {
  sc_na <- sc; sc_na$agecat[1] <- NA

  expect_error(
    handle_na_for_ipwm(sc_na, sp1, ~agecat, na_mode = "fail"),
    "na.fail: NA found in p_formula variables in sc.",
    fixed = TRUE
  )
})

test_that("handle_na_for_ipwm (fail): errors when NA in sp", {
  sp_na <- sp1; sp_na$agecat[1] <- NA

  expect_error(
    handle_na_for_ipwm(sc, sp_na, ~agecat, na_mode = "fail"),
    "na.fail: NA found in p_formula variables in sp.",
    fixed = TRUE
  )
})

test_that("handle_na_for_ipwm (pass): keeps all rows despite NA", {
  sc_na <- sc;  sc_na$agecat[1:5] <- NA
  sp_na <- sp1; sp_na$agecat[1:3] <- NA

  res <- handle_na_for_ipwm(sc_na, sp_na, ~agecat, na_mode = "pass")

  expect_equal(nrow(res$sc), nrow(sc))
  expect_equal(nrow(res$sp), nrow(sp1))
  expect_true(all(res$keep_sc))
  expect_true(all(res$keep_sp))
})

test_that("handle_na_for_ipwm: no NA -> all rows kept, na_action NULL", {
  res <- handle_na_for_ipwm(sc, sp1, ~agecat, na_mode = "omit")

  expect_true(all(res$keep_sc))
  expect_true(all(res$keep_sp))
  expect_null(res$na_action)
})

test_that("handle_na_for_ipwm (multi-ref): each sp handled independently", {
  sp_na1 <- sp1; sp_na1$agecat[1:2] <- NA
  sp_list <- list(sp_na1, sp2)
  fml_list <- list(~agecat, ~agecat)

  res <- handle_na_for_ipwm(sc, sp_list, fml_list, na_mode = "omit")

  expect_equal(sum(!res$keep_sp[[1]]), 2)
  expect_true(all(res$keep_sp[[2]]))
  expect_equal(nrow(res$sp[[1]]), nrow(sp1) - 2)
  expect_equal(nrow(res$sp[[2]]), nrow(sp2))
})


# ── process_na_build ─────────────────────────────────────────────────────────

test_that("process_na_build: na_summary is NULL when no NAs", {
  res <- process_na_build(
    sc        = sc,
    sp        = sp1,
    sp_des    = des1,
    p_formula = ~agecat,
    na.action = stats::na.omit,
    n_ref     = 1L,
    verbose   = FALSE
  )

  expect_null(res$na_summary)
})

test_that("process_na_build: na_summary has correct class and counts", {
  sc_na <- sc;  sc_na$agecat[1:3] <- NA
  sp_na <- sp1; sp_na$agecat[1:2] <- NA
  des_na <- survey::svydesign(
    ids     = ~psu_sp1,
    strata  = ~strata_sp1,
    weights = ~wts_sp1,
    data    = sp_na,
    nest    = TRUE
  )

  res <- process_na_build(
    sc        = sc_na,
    sp        = sp_na,
    sp_des    = des_na,
    p_formula = ~agecat,
    na.action = stats::na.omit,
    n_ref     = 1L,
    verbose   = FALSE
  )

  expect_s3_class(res$na_summary, "pw_na_summary")
  expect_equal(res$na_summary$sc$n_orig,     nrow(sc_na))
  expect_equal(res$na_summary$sc$n_excluded, 3)
  expect_equal(res$na_summary$sp$n_orig,     nrow(sp_na))
  expect_equal(res$na_summary$sp$n_excluded, 2)
})

test_that("process_na_build: sp_des is subsetted after NA removal", {
  sp_na <- sp1; sp_na$agecat[1:4] <- NA
  des_na <- survey::svydesign(
    ids     = ~psu_sp1,
    strata  = ~strata_sp1,
    weights = ~wts_sp1,
    data    = sp_na,
    nest    = TRUE
  )

  res <- process_na_build(
    sc        = sc,
    sp        = sp_na,
    sp_des    = des_na,
    p_formula = ~agecat,
    na.action = stats::na.omit,
    n_ref     = 1L,
    verbose   = FALSE
  )

  expect_equal(nrow(res$sp_des$variables), nrow(sp1) - 4)
})

test_that("process_na_build: na.fail errors when NA present", {
  sc_na <- sc; sc_na$agecat[1] <- NA

  expect_error(
    process_na_build(
      sc        = sc_na,
      sp        = sp1,
      sp_des    = des1,
      p_formula = ~agecat,
      na.action = stats::na.fail,
      n_ref     = 1L,
      verbose   = FALSE
    ),
    "na.fail: NA found in p_formula variables in sc.",
    fixed = TRUE
  )
})

test_that("process_na_build: verbose = TRUE emits messages", {
  sc_na <- sc; sc_na$agecat[1] <- NA

  expect_message(
    process_na_build(
      sc        = sc_na,
      sp        = sp1,
      sp_des    = des1,
      p_formula = ~agecat,
      na.action = stats::na.omit,
      n_ref     = 1L,
      verbose   = TRUE
    )
  )
})
