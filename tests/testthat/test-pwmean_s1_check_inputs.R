# Setup ----

data(sc)

# helper: build a minimal pw_fit around a data frame
make_pw_fit <- function(raw_sc, weights = NULL, keep_sc = NULL, Xc = NULL,
                        method = "alp", D = NULL, S_beta = NULL) {
  n      <- nrow(raw_sc)
  if (is.null(keep_sc)) keep_sc <- rep(TRUE, n)
  n_keep <- sum(keep_sc)
  if (is.null(weights)) weights <- rep(1.0, n_keep)
  if (is.null(Xc))      Xc      <- matrix(1.0, nrow = n_keep, ncol = 1L)
  if (is.null(D))       D       <- matrix(0.0, nrow = 1L,     ncol = 1L)
  if (is.null(S_beta))  S_beta  <- matrix(1.0, nrow = 1L,     ncol = 1L)
  structure(
    list(
      pseudo_weights = weights,
      method         = method,
      internal = list(
        raw_sc = raw_sc,
        na     = list(keep_sc = keep_sc),
        Xc     = Xc,
        D      = D,
        S_beta = S_beta
      )
    ),
    class = "pw_fit"
  )
}

fit <- make_pw_fit(sc)


# valid inputs ----

test_that("check_ipwm_inputs_estimate: valid input without zcol returns invisible TRUE", {
  expect_invisible(check_ipwm_inputs_estimate(fit, y = "psa_level"))
})

test_that("check_ipwm_inputs_estimate: valid input with factor zcol returns invisible TRUE", {
  expect_invisible(check_ipwm_inputs_estimate(fit, y = "psa_level", zcol = "race"))
})

test_that("check_ipwm_inputs_estimate: valid input with logical zcol returns invisible TRUE", {
  sc_log          <- sc
  sc_log$is_obese <- sc_log$BMI == "Obese"
  fit_log         <- make_pw_fit(sc_log)
  expect_invisible(check_ipwm_inputs_estimate(fit_log, y = "psa_level", zcol = "is_obese"))
})

test_that("check_ipwm_inputs_estimate: rejects logical zcol with only one unique value", {
  sc_log          <- sc
  sc_log$all_true <- rep(TRUE, nrow(sc))
  fit_log         <- make_pw_fit(sc_log)
  expect_error(
    check_ipwm_inputs_estimate(fit_log, y = "psa_level", zcol = "all_true"),
    "must contain both TRUE and FALSE.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: valid input with character zcol returns invisible TRUE", {
  sc_chr          <- sc
  sc_chr$race_chr <- as.character(sc_chr$race)
  fit_chr         <- make_pw_fit(sc_chr)
  expect_invisible(check_ipwm_inputs_estimate(fit_chr, y = "psa_level", zcol = "race_chr"))
})

test_that("check_ipwm_inputs_estimate: valid input with binary numeric zcol returns invisible TRUE", {
  sc_bin            <- sc
  sc_bin$has_diabetes <- as.integer(sc_bin$diabetes == "1")
  fit_bin           <- make_pw_fit(sc_bin)
  expect_invisible(check_ipwm_inputs_estimate(fit_bin, y = "psa_level", zcol = "has_diabetes"))
})

test_that("check_ipwm_inputs_estimate: valid input with factor y returns invisible TRUE", {
  expect_invisible(check_ipwm_inputs_estimate(fit, y = "diabetes"))
})


# build object checks ----

test_that("check_ipwm_inputs_estimate: rejects non-pw_fit object", {
  expect_error(
    check_ipwm_inputs_estimate(list(), y = "psa_level"),
    "'object' must be of class \"pw_fit\".",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing raw_sc", {
  bad                    <- fit
  bad$internal$raw_sc   <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$internal$raw_sc is missing.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing keep_sc", {
  bad                       <- fit
  bad$internal$na$keep_sc  <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$internal$na$keep_sc is missing.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing pseudo_weights", {
  bad                  <- fit
  bad$pseudo_weights   <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$pseudo_weights is missing.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing Xc", {
  bad              <- fit
  bad$internal$Xc  <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$internal$Xc is missing.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects NULL method", {
  bad         <- fit
  bad$method  <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$method must be a single character string.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects non-character method", {
  bad         <- fit
  bad$method  <- 1L
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$method must be a single character string.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing D", {
  bad              <- fit
  bad$internal$D   <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$internal$D is missing.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects missing S_beta", {
  bad                  <- fit
  bad$internal$S_beta  <- NULL
  expect_error(
    check_ipwm_inputs_estimate(bad, y = "psa_level"),
    "build$internal$S_beta is missing.",
    fixed = TRUE
  )
})


# y checks ----

test_that("check_ipwm_inputs_estimate: rejects non-character y", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = 1L),
    "'y' must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects NA y", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = NA_character_),
    "'y' must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects empty string y", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = ""),
    "'y' must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects y not found in sc", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = "not_a_column"),
    "Outcome variable 'not_a_column' not found in sc.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects unsupported y type", {
  sc_chr          <- sc
  sc_chr$race_chr <- as.character(sc_chr$race)
  fit_chr         <- make_pw_fit(sc_chr)
  expect_error(
    check_ipwm_inputs_estimate(fit_chr, y = "race_chr"),
    "Outcome variable 'race_chr' must be numeric or factor.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects factor y with no non-missing level", {
  sc_empty        <- sc
  sc_empty$empty  <- factor(rep(NA_character_, nrow(sc)))
  fit_empty       <- make_pw_fit(sc_empty)
  expect_error(
    check_ipwm_inputs_estimate(fit_empty, y = "empty"),
    "must contain at least one non-missing level.",
    fixed = TRUE
  )
})


# zcol checks ----

test_that("check_ipwm_inputs_estimate: rejects non-character zcol", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = "psa_level", zcol = 1L),
    "'zcol' must be a single non-empty character string or NULL.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects NA zcol", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = "psa_level", zcol = NA_character_),
    "'zcol' must be a single non-empty character string or NULL.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects empty string zcol", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = "psa_level", zcol = ""),
    "'zcol' must be a single non-empty character string or NULL.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects zcol not found in sc", {
  expect_error(
    check_ipwm_inputs_estimate(fit, y = "psa_level", zcol = "not_a_column"),
    "Domain variable 'not_a_column' not found in sc.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects factor zcol with only one level", {
  sc_one             <- sc
  sc_one$single_lev  <- factor(rep("A", nrow(sc)))
  fit_one            <- make_pw_fit(sc_one)
  expect_error(
    check_ipwm_inputs_estimate(fit_one, y = "psa_level", zcol = "single_lev"),
    "must contain at least two non-missing levels.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects character zcol with only one non-missing level", {
  sc_one      <- sc
  sc_one$grp  <- rep("A", nrow(sc))
  fit_one     <- make_pw_fit(sc_one)
  expect_error(
    check_ipwm_inputs_estimate(fit_one, y = "psa_level", zcol = "grp"),
    "must contain at least two non-missing levels.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: character zcol trimming treats ' A' and 'A' as same level", {
  sc_trim      <- sc
  sc_trim$grp  <- rep(c(" A", "A"), length.out = nrow(sc))
  fit_trim     <- make_pw_fit(sc_trim)
  expect_error(
    check_ipwm_inputs_estimate(fit_trim, y = "psa_level", zcol = "grp"),
    "must contain at least two non-missing levels.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects non-binary numeric zcol", {
  sc_num        <- sc
  sc_num$score  <- c(0, 1, 2, rep(0, nrow(sc) - 3))
  fit_num       <- make_pw_fit(sc_num)
  expect_error(
    check_ipwm_inputs_estimate(fit_num, y = "psa_level", zcol = "score"),
    "is numeric but not binary.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects numeric zcol containing only 0", {
  sc_zero           <- sc
  sc_zero$all_zeros <- rep(0L, nrow(sc))
  fit_zero          <- make_pw_fit(sc_zero)
  expect_error(
    check_ipwm_inputs_estimate(fit_zero, y = "psa_level", zcol = "all_zeros"),
    "must contain both 0 and 1.",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects unsupported zcol type", {
  sc_bad          <- sc
  sc_bad$bad_col  <- as.list(rep(1, nrow(sc)))
  fit_bad         <- make_pw_fit(sc_bad)
  expect_error(
    check_ipwm_inputs_estimate(fit_bad, y = "psa_level", zcol = "bad_col"),
    "must be one of the following",
    fixed = TRUE
  )
})


# zcol level check against sc_keep (build-stage filtered rows) ----
#
# Step 3 validates zcol against raw_sc. Step 4 repeats the level-count check
# against sc_keep so the user gets a clear error at input-check time rather
# than an uninformative label-mismatch error from assemble_output later.

test_that("check_ipwm_inputs_estimate: rejects factor zcol with one level after build-stage filtering", {
  n          <- nrow(sc)
  sc_grp     <- sc
  sc_grp$grp <- factor(rep(c("A", "B"), length.out = n))
  keep       <- sc_grp$grp != "A"   # remove all A rows from sc_keep
  fit_keep   <- make_pw_fit(sc_grp, keep_sc = keep)
  expect_error(
    check_ipwm_inputs_estimate(fit_keep, y = "psa_level", zcol = "grp"),
    "fewer than two non-missing levels in the build-stage complete cases",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects character zcol with one level after build-stage filtering", {
  n          <- nrow(sc)
  sc_grp     <- sc
  sc_grp$grp <- rep(c("A", "B"), length.out = n)
  keep       <- sc_grp$grp != "A"
  fit_keep   <- make_pw_fit(sc_grp, keep_sc = keep)
  expect_error(
    check_ipwm_inputs_estimate(fit_keep, y = "psa_level", zcol = "grp"),
    "fewer than two non-missing levels in the build-stage complete cases",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects logical zcol with one value after build-stage filtering", {
  n           <- nrow(sc)
  sc_log      <- sc
  sc_log$flag <- rep(c(TRUE, FALSE), length.out = n)
  keep        <- !sc_log$flag   # remove all TRUE rows from sc_keep
  fit_keep    <- make_pw_fit(sc_log, keep_sc = keep)
  expect_error(
    check_ipwm_inputs_estimate(fit_keep, y = "psa_level", zcol = "flag"),
    "fewer than two non-missing levels in the build-stage complete cases",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: rejects binary numeric zcol with one value after build-stage filtering", {
  n        <- nrow(sc)
  sc_bin   <- sc
  sc_bin$flag <- rep(c(0L, 1L), length.out = n)
  keep     <- sc_bin$flag == 0L   # remove all 1-rows from sc_keep
  fit_keep <- make_pw_fit(sc_bin, keep_sc = keep)
  expect_error(
    check_ipwm_inputs_estimate(fit_keep, y = "psa_level", zcol = "flag"),
    "fewer than two non-missing levels in the build-stage complete cases",
    fixed = TRUE
  )
})

test_that("check_ipwm_inputs_estimate: passes when zcol retains >= 2 levels after build-stage filtering", {
  n          <- nrow(sc)
  sc_grp     <- sc
  sc_grp$grp <- factor(rep(c("A", "B"), length.out = n))
  keep       <- rep(TRUE, n)
  keep[1L]   <- FALSE   # drop one row only; both levels remain
  fit_keep   <- make_pw_fit(sc_grp, keep_sc = keep)
  expect_invisible(check_ipwm_inputs_estimate(fit_keep, y = "psa_level", zcol = "grp"))
})
