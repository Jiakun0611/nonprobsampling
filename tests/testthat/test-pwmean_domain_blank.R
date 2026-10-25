make_domain_blank_data <- function() {
  data.frame(
    x = seq(-1, 1, length.out = 10),
    y = c(1, 4, 9, 16, 25, 36, 49, NA_real_, 81, 100),
    domain = c(" A ", "B", "", " \t ", "A", " B ", NA, "A", "B", "A"),
    category = factor(rep(c("yes", "no"), 5))
  )
}

test_that("blank domains are removed with aligned data and original row indices", {
  df <- make_domain_blank_data()
  input <- list(
    sc = df,
    X = cbind(1, df$x),
    w = seq_len(nrow(df)) + 10,
    idx_keep = seq.int(2L, 20L, by = 2L)
  )
  original <- input
  keep <- c(1L, 2L, 5L, 6L, 9L, 10L)

  for (mode in c("omit", "exclude")) {
    out <- process_na_yz(input, "y", "domain", na.action = mode)
    expect_equal(out$Y, df$y[keep])
    expect_equal(out$X, input$X[keep, , drop = FALSE])
    expect_equal(out$w, input$w[keep])
    expect_equal(out$sc$domain, c("A", "B", "A", "B", "B", "A"))
    expect_equal(out$na_info$omitted_raw, c(6L, 8L, 14L, 16L))
    expect_equal(out$na_info$kept_raw, input$idx_keep[keep])
    expect_equal(out$na_info$n_omitted, 4L)
    expect_equal(out$na_info$n_used, 6L)
    expect_s3_class(out$na_info$na_action, mode)
    expect_equal(as.integer(out$na_info$na_action), c(6L, 8L, 14L, 16L))
    expect_false(anyNA(out$domain$indicators))
  }
  expect_identical(input, original)
})

test_that("na.fail detects empty and whitespace-only domains before estimation", {
  for (blank in c("", " ", "\t", " \t ")) {
    df <- data.frame(y = 1:5, domain = c("A", "B", blank, "A", "B"))
    input <- list(sc = df, X = matrix(1, 5, 1), w = rep(10, 5), idx_keep = 1:5)
    expect_error(
      process_na_yz(input, "y", "domain", na.action = stats::na.fail),
      "Missing values detected in `y` or `zcol`.", fixed = TRUE
    )
  }
})

test_that("all blank domains produce a no-complete-cases error", {
  df <- data.frame(y = 1:3, domain = c("", " ", "\t"))
  input <- list(sc = df, X = matrix(1, 3, 1), w = rep(10, 3), idx_keep = 1:3)
  expect_error(
    process_na_yz(input, "y", "domain"),
    "No complete cases remain", fixed = TRUE
  )
})

test_that("unweighted domain estimates omit blanks before computing moments", {
  df <- make_domain_blank_data()
  original <- df
  out <- naive_mean(df, "domain", "y")
  expect_equal(out$labels, c("domain = A", "domain = B"))
  expect_equal(out$estimates[[1]], list(
    mean = mean(c(1, 25, 100)), variance = stats::var(c(1, 25, 100)) / 3
  ))
  expect_equal(out$estimates[[2]], list(
    mean = mean(c(4, 36, 81)), variance = stats::var(c(4, 36, 81)) / 3
  ))
  expect_identical(df, original)
})

test_that("blank-domain public estimates match explicitly cleaned data", {
  df <- make_domain_blank_data()
  df$z <- df$x^2
  # Exercise original row numbering after a build-stage omission as well.
  df$x[2L] <- NA_real_
  clean <- df
  clean$domain <- c("A", "B", NA, NA, "A", "B", NA, "A", "B", "A")
  ref <- data.frame(x = seq(-1, 1, length.out = 10), wt = 10)
  ref$z <- ref$x^2
  des <- survey::svydesign(ids = ~1, weights = ~wt, data = ref)

  for (method in c("calibration", "alp", "clw", "multi")) {
    references <- if (method == "multi") list(des, des) else list(des)
    formula <- if (method == "multi") list(~x, ~z) else ~x
    fit <- est_pw(c(list(df), references), p_formula = formula, method = method)
    expected_fit <- est_pw(c(list(clean), references), p_formula = formula, method = method)
    original <- fit

    for (outcome in c("y", "category")) {
      for (mode in c("omit", "exclude")) {
        out <- pwmean(fit, outcome, "domain", na.action = mode)
        expected <- pwmean(expected_fit, outcome, "domain", na.action = mode)
        expect_equal(out$estimates, expected$estimates)
        expect_true(all(is.finite(as.matrix(out$estimates[, c(
          "unweighted_mean", "unweighted_se", "adjusted_mean", "adjusted_se"
        )]))))
        expect_identical(stats::na.action(out), stats::na.action(expected))
        expect_s3_class(stats::na.action(out), mode)
        dropped <- if (outcome == "y") c(3L, 4L, 7L, 8L) else c(3L, 4L, 7L)
        expect_equal(as.integer(stats::na.action(out)), dropped)
      }
      expect_error(
        pwmean(fit, outcome, "domain", na.action = stats::na.fail),
        "Step 3 (process_na_yz) failed: Missing values detected", fixed = TRUE
      )
    }
    expect_identical(fit, original)
  }
})
