data("sp1")

# check nh (PSUs per stratum)
nh <- tapply(sp1$psu_sp1, sp1$strata_sp1, function(x) length(unique(x)))
summary(nh)

# Confirm no lonely PSU strata
any(nh <= 1)


make_bootstrap_weights <- function(
    sp,
    weight_col,
    psu_col    = "psu",
    strata_col = "strata",
    R          = 500,
    mh         = NULL,
    seed       = 123,
    check_negative = TRUE
) {
  stopifnot(is.data.frame(sp))
  stopifnot(all(c(weight_col, psu_col, strata_col) %in% names(sp)))

  set.seed(seed)

  w  <- sp[[weight_col]]
  ps <- as.character(sp[[psu_col]])
  st <- as.character(sp[[strata_col]])

  nh_by_stratum <- tapply(ps, st, function(x) length(unique(x)))

  bad <- which(nh_by_stratum <= 1)
  if (length(bad) > 0) {
    stop(
      "Some strata have nh <= 1: ",
      paste(names(nh_by_stratum)[bad], collapse = ", ")
    )
  }

  if (is.null(mh)) {
    mh_by_stratum <- nh_by_stratum - 1
  } else if (length(mh) == 1 && is.numeric(mh)) {
    mh_by_stratum <- rep(mh, length(nh_by_stratum))
    names(mh_by_stratum) <- names(nh_by_stratum)
  } else {
    mh_by_stratum <- mh
  }

  mh_by_stratum <- mh_by_stratum[names(nh_by_stratum)]

  if (any(is.na(mh_by_stratum))) {
    stop("`mh` must be supplied for every stratum.")
  }

  mh_by_stratum <- pmax(1, pmin(mh_by_stratum, nh_by_stratum - 1))
  mh_by_stratum <- setNames(as.numeric(mh_by_stratum), names(nh_by_stratum))

  n <- nrow(sp)
  repW <- matrix(NA_real_, nrow = n, ncol = R)
  strata_levels <- names(nh_by_stratum)

  for (t in seq_len(R)) {
    wt <- w

    for (h in strata_levels) {
      idx_h <- which(st == h)
      psu_h <- unique(ps[idx_h])

      nh   <- length(psu_h)
      mh_h <- as.integer(mh_by_stratum[h])

      W_hi <- tapply(w[idx_h], ps[idx_h], sum)

      draw <- sample(psu_h, size = mh_h, replace = TRUE)

      r <- tabulate(match(draw, psu_h), nbins = nh)
      names(r) <- psu_h

      g <- sqrt(mh_h / (nh - 1))

      mult_psu <- 1 - g + g * (nh / mh_h) * r

      ratio <- mult_psu[ps[idx_h]]

      wt[idx_h] <- w[idx_h] * ratio
    }

    if (check_negative && any(!is.finite(wt) | wt < 0)) {
      stop("Replicate ", t, " produced non-finite or negative weights.")
    }

    repW[, t] <- wt
  }

  colnames(repW) <- paste0("bw", seq_len(R))
  repW
}

repW_500 <- make_bootstrap_weights(
  sp         = sp1,
  weight_col = "wts_sp1",
  psu_col    = "psu_sp1",
  strata_col = "strata_sp1",
  R          = 500,
  seed       = 2026
)

dim(repW_500)  # nrow(sp1) x 500
head(repW_500[, 1:10])

base_total <- sum(sp1$wts_sp1)
rep_totals <- colSums(repW_500)
head(rep_totals)

c(base_total = base_total,
  rep_total_mean   = mean(rep_totals),
  rep_sd     = sd(rep_totals))

sp1_cols <- sp1[, !names(sp1) %in% c("psu_sp1", "strata_sp1")]
sp1_bootstrap <- cbind(sp1_cols, repW_500)
head(sp1_bootstrap[, 1:20])


