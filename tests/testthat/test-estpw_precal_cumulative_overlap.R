# Exercise the caller as well as the cumulative calibration helper.
make_precal_overlap <- function(global = FALSE, categorical = FALSE) {
  dat <- expand.grid(x = 0:1, y = 0:1, g = 0:1)
  if (categorical) dat$y <- factor(dat$y, levels = 0:1)
  refs <- list(
    first = data.frame(dat[c("x", "g")], w = 1 + dat$x),
    second = data.frame(dat[c("x", "y", "g")], w = 1 + as.integer(dat$y)),
    third = data.frame(dat[c("y", "g")], w = 3 - as.integer(dat$y))
  )
  formulas <- if (global) list(~x + g, ~x + y + g, ~y + g) else {
    list(~x, ~x + y, ~y)
  }
  list(sc = dat, sp = refs, formulas = formulas)
}

run_precal_overlap <- function(dat, order = "given", enabled = TRUE) {
  process_p_formula(
    dat$sc, dat$sp, weight = rep("w", length(dat$sp)),
    p_formula = dat$formulas, Pre.calibration = enabled, sp_order = order
  )
}

test_that("cumulative precalibration retains chain overlap without a global intersection", {
  dat <- make_precal_overlap()
  out <- run_precal_overlap(dat)
  expect_equal(out$sp$first$w, dat$sp$first$w)
  expect_equal(sum(out$sp$second$w), 12)
  expect_equal(sum(out$sp$second$w * dat$sp$second$x), 8)
  expect_equal(sum(out$sp$second$w * dat$sp$second$y), 8)
  expect_equal(sum(out$sp$third$w), 12)
  expect_equal(sum(out$sp$third$w * dat$sp$third$y), 8)
})

test_that("global overlap does not suppress locally shared variables", {
  dat <- make_precal_overlap(global = TRUE)
  out <- run_precal_overlap(dat)
  for (j in 2:3) {
    expect_equal(sum(out$sp[[j]]$w * dat$sp[[j]]$g), 6)
  }
  expect_equal(sum(out$sp$second$w * dat$sp$second$x), 8)
  expect_equal(sum(out$sp$third$w * dat$sp$third$y), 8)
})

test_that("factor totals are passed from calibrated intermediate samples", {
  dat <- make_precal_overlap(categorical = TRUE)
  out <- run_precal_overlap(dat)
  target <- sum(out$sp$second$w * (dat$sp$second$y == "1"))
  expect_equal(target, 7.2)
  expect_equal(sum(out$sp$third$w * (dat$sp$third$y == "1")), target)
})

test_that("cumulative overlap follows given order and size order", {
  dat <- make_precal_overlap()
  # Make the last sample largest, then compare with an explicit reversal.
  dat$sp$third <- dat$sp$third[rep(seq_len(8), 2), ]
  dat$sp$third$w <- dat$sp$third$w / 2
  given <- run_precal_overlap(dat)
  sized <- run_precal_overlap(dat, order = "size")
  expect_equal(sum(given$sp$third$w * dat$sp$third$y), 8)
  expect_equal(sized$sp$third$w, dat$sp$third$w)
  expect_equal(sum(sized$sp$second$w * dat$sp$second$y), 8)
  expect_equal(sum(sized$sp$first$w * dat$sp$first$x), 40 / 3)
  expect_equal(sum(sized$sp$second$w * dat$sp$second$x), 40 / 3)
  # Tied remaining sample sizes retain their input order: third, first, second.
  explicit <- dat
  explicit$sp <- dat$sp[c(3, 1, 2)]
  explicit$formulas <- dat$formulas[c(3, 1, 2)]
  reordered <- run_precal_overlap(explicit)
  expect_named(sized$sp, names(dat$sp))
  for (nm in names(dat$sp)) expect_equal(sized$sp[[nm]], reordered$sp[[nm]])
})

test_that("disjoint formulas calibrate only the weight total", {
  dat <- make_precal_overlap()
  dat$sp <- dat$sp[c(1, 3)]
  dat$formulas <- list(~x, ~y)
  out <- run_precal_overlap(dat)
  expect_equal(out$sp$third$w, dat$sp$third$w * 12 / 20)
  expect_match(out$log_messages[2], "survey weights total only", fixed = TRUE)
})

test_that("targets survive an intervening survey without the variable", {
  dat <- make_precal_overlap()
  dat$sp <- list(dat$sp$first, dat$sp$third, dat$sp$second)
  dat$formulas <- list(~x, ~y, ~x + y)
  out <- run_precal_overlap(dat)
  expect_equal(sum(out$sp[[3]]$w * dat$sp[[3]]$x), 8)
  expect_equal(sum(out$sp[[3]]$w * dat$sp[[3]]$y), 4.8)
})

test_that("disabled precalibration and one-reference processing preserve weights", {
  dat <- make_precal_overlap()
  out <- run_precal_overlap(dat, enabled = FALSE)
  for (nm in names(dat$sp)) expect_equal(out$sp[[nm]]$w, dat$sp[[nm]]$w)
  one <- process_p_formula(dat$sc, dat$sp$first, "w", ~x)
  expect_equal(one$sp$w, dat$sp$first$w)
  dat$sp <- dat$sp[1:2]
  dat$formulas <- dat$formulas[1:2]
  two <- run_precal_overlap(dat)
  expect_equal(sum(two$sp$second$w * dat$sp$second$x), 8)
})
