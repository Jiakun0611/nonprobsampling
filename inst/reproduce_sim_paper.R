# Simulation study based on Landsman et al. (2026), Statistics in Medicine,
# doi:10.1002/sim.70403.
#
# This script reproduces selected continuous-outcome results from the paper:
# Table 1, Case 0, for the one-reference ALP, CLW, and calibration methods;
# and Table 2, Case PC.1, for the multi-reference calibration method.
#
# The reported metrics follow the paper: RB(%), v(*100), MSE(*100), SER, and CP.

library(nonprobsampling)
library(sampling)
library(survey)
library(knitr)

old_options <- options(
  scipen = 999,               # no scientific notation
  digits = 4
)

# -------- Simulation rounds and seeds ------------
# Reduce n_sim for a quick trial run; set to 4000 to reproduce the paper.
set.seed(123456)
n_sim <- 4000
seeds_sc <- sample.int(1e6, n_sim)
seeds_sp <- sample.int(1e6, n_sim)

# -------- Helper functions ------------

# True-weight estimator using known participation probabilities.
# sc must have columns 'y' and 'wt_sc' (= 1 / pi_c).
# Returns c(mean, se) under Poisson sampling.
TW.sc <- function(sc) {
  w <- sc$wt_sc
  y <- sc$y
  m <- sum(w * y) / sum(w)
  v <- sum(w * (w - 1) * (y - m)^2) / sum(w)^2
  c(m, sqrt(v))
}

# Simulation evaluation metrics.
#
# mu:      numeric vector of point estimates across simulation replications,
#          i.e., mu_hat^(1), ..., mu_hat^(B).
# se:      numeric vector of estimated standard errors across replications.
# mu_true: true population mean.
#
# Returns:
#   RB(%)      relative bias: 100 * mean((mu_hat - mu_true) / mu_true).
#   v(*100)    average estimated analytic variance * 100: mean(se^2) * 100.
#   MSE(*100)  mean squared error * 100: mean((mu_hat - mu_true)^2) * 100.
#   SER        ratio of estimated SE to simulated SE: mean(se) / sd(mu).
#   CP         empirical 95% Wald confidence interval coverage probability.

evaluation <- function(mu, se, mu_true) {
  RB  <- mean((mu - mu_true) / mu_true)
  MSE <- mean((mu - mu_true)^2)
  ESE <- mean(se)
  SSE <- sd(mu)
  SER <- ESE / SSE
  CP  <- mean(mu_true >= mu - 1.96 * se & mu_true <= mu + 1.96 * se)
  return(c(RB * 100, v = mean(se^2) * 100, MSE * 100, SER, CP))
}

# -------- Finite population ------------
N <- 500000

v1 <- rbinom(N, size = 1, prob = 0.5)
v2 <- runif(N, min = 0, max = 2)
v3 <- rexp(N, rate = 1)
v4 <- rchisq(N, df = 4)

x1 <- v1
x2 <- v2 + 0.3 * x1
x3 <- v3 + 0.2 * (x1 + x2)
x4 <- v4 + 0.1 * (x1 + x2 + x3)

mu  <- -x1 - x2 + x3 + x4
y   <- mu + rnorm(N)
mu_true <- mean(y)

# -------- Sampling designs ------------

# sp1: Poisson sampling (n ≈ 12500)
np1 <- 12500
a <- x3 + 0.03 * y
rng <- range(a)
cnst_sp1 <- (rng[2] - 20 * rng[1]) / 19
q <- cnst_sp1 + a
pi_p1 <- np1 * q / sum(q)
di1   <- 1 / pi_p1

# sp2: randomized systematic PPS (n = 25000)
np2    <- 25000
const2 <- 0.05
z      <- const2 + x2
pi_p2  <- np2 * z / sum(z)
di2    <- 1 / pi_p2

# Nonprobability sample sc: Poisson sampling (n ≈ 2500)
nc  <- 2500
eta <- 0.18 * x1 + 0.18 * x2 - 0.27 * x3 - 0.27 * x4
beta0 <- log(nc / sum(exp(eta)))
pi_c  <- exp(eta + beta0)

# Finite population frame
fp <- data.frame(
  x1     = x1, x2 = x2, x3 = x3, x4 = x4,
  y      = y,
  pi_sp1 = pi_p1, wt_sp1 = di1,
  pi_sp2 = pi_p2, wt_sp2 = di2,
  pi_c   = pi_c,  wt_sc  = 1 / pi_c
)


########################################
# Simulation 1: sp1 (Poisson) as reference
########################################

result_sp1 <- matrix(NA, nrow = n_sim, ncol = 10)

for (i in seq_len(n_sim)) {

  # nonprobability sample
  set.seed(seeds_sc[i])
  sc <- fp[rbinom(N, 1, pi_c) == 1,
           c("x1", "x2", "x3", "x4", "y", "wt_sc")]

  # probability reference sample sp1 (Poisson)
  set.seed(seeds_sp[i])
  sp1 <- fp[rbinom(N, 1, pi_p1) == 1,
            c("x1", "x2", "x3", "x4", "wt_sp1")]
  sp1$pi_sp1 <- 1 / sp1$wt_sp1

  des_sp1 <- svydesign(ids = ~1, probs = ~pi_sp1, data = sp1)

  # true-weight estimator
  result_tw <- TW.sc(sc)

  # pseudo-weight estimation: ALP, CLW, Calibration
  fit_alp  <- est_pw(data = list(sc, des_sp1), method = "alp")
  fit_clw  <- est_pw(data = list(sc, des_sp1), method = "clw")
  fit_cali <- est_pw(data = list(sc, des_sp1), method = "calibration")

  res_alp  <- pwmean(fit_alp,  y = "y")
  res_clw  <- pwmean(fit_clw,  y = "y")
  res_cali <- pwmean(fit_cali, y = "y")

  # naive (from any fit)
  result_sp1[i, 1:2]  <- c(res_alp$estimates$unweighted_mean,
                            res_alp$estimates$unweighted_se)
  # true weight
  result_sp1[i, 3:4]  <- result_tw
  # ALP
  result_sp1[i, 5:6]  <- c(res_alp$estimates$adjusted_mean,
                            res_alp$estimates$adjusted_se)
  # CLW
  result_sp1[i, 7:8]  <- c(res_clw$estimates$adjusted_mean,
                            res_clw$estimates$adjusted_se)
  # Calibration
  result_sp1[i, 9:10] <- c(res_cali$estimates$adjusted_mean,
                            res_cali$estimates$adjusted_se)

  if (i %% 100 == 0) cat(i, " ")
}

table_sp1 <- matrix(NA, nrow = 5, ncol = 5)
colnames(table_sp1) <- c("RB(%)", "v(*100)", "MSE(*100)", "SER", "CP")
rownames(table_sp1) <- c("Naive", "True weight", "ALP", "CLW", "Calibration")

table_sp1[1, ] <- evaluation(result_sp1[, 1],  result_sp1[, 2],  mu_true)
table_sp1[2, ] <- evaluation(result_sp1[, 3],  result_sp1[, 4],  mu_true)
table_sp1[3, ] <- evaluation(result_sp1[, 5],  result_sp1[, 6],  mu_true)
table_sp1[4, ] <- evaluation(result_sp1[, 7],  result_sp1[, 8],  mu_true)
table_sp1[5, ] <- evaluation(result_sp1[, 9],  result_sp1[, 10], mu_true)


########################################
# Simulation 2: sp2 (systematic PPS) as reference
########################################

result_sp2 <- matrix(NA, nrow = n_sim, ncol = 10)

for (i in seq_len(n_sim)) {

  # nonprobability sample
  set.seed(seeds_sc[i])
  sc <- fp[rbinom(N, 1, pi_c) == 1,
           c("x1", "x2", "x3", "x4", "y", "wt_sc")]

  # probability reference sample sp2 (randomized systematic PPS)
  set.seed(seeds_sp[i])
  s2  <- UPrandomsystematic(pi_p2)
  sp2 <- fp[s2 == 1, c("x1", "x2", "x3", "x4", "wt_sp2", "pi_sp2")]

  des_sp2 <- svydesign(ids = ~1, fpc = ~pi_sp2, data = sp2, pps = "brewer")

  # true-weight estimator
  result_tw <- TW.sc(sc)

  # pseudo-weight estimation: ALP, CLW, Calibration
  fit_alp  <- est_pw(data = list(sc, des_sp2), method = "alp")
  fit_clw  <- est_pw(data = list(sc, des_sp2), method = "clw")
  fit_cali <- est_pw(data = list(sc, des_sp2), method = "calibration")

  res_alp  <- pwmean(fit_alp,  y = "y")
  res_clw  <- pwmean(fit_clw,  y = "y")
  res_cali <- pwmean(fit_cali, y = "y")

  # naive
  result_sp2[i, 1:2]  <- c(res_alp$estimates$unweighted_mean,
                            res_alp$estimates$unweighted_se)
  # true weight
  result_sp2[i, 3:4]  <- result_tw
  # ALP
  result_sp2[i, 5:6]  <- c(res_alp$estimates$adjusted_mean,
                            res_alp$estimates$adjusted_se)
  # CLW
  result_sp2[i, 7:8]  <- c(res_clw$estimates$adjusted_mean,
                            res_clw$estimates$adjusted_se)
  # Calibration
  result_sp2[i, 9:10] <- c(res_cali$estimates$adjusted_mean,
                            res_cali$estimates$adjusted_se)

  if (i %% 100 == 0) cat(i, " ")
}

table_sp2 <- matrix(NA, nrow = 5, ncol = 5)
colnames(table_sp2) <- c("RB(%)", "v(*100)", "MSE(*100)", "SER", "CP")
rownames(table_sp2) <- c("Naive", "True weight", "ALP", "CLW", "Calibration")

table_sp2[1, ] <- evaluation(result_sp2[, 1],  result_sp2[, 2],  mu_true)
table_sp2[2, ] <- evaluation(result_sp2[, 3],  result_sp2[, 4],  mu_true)
table_sp2[3, ] <- evaluation(result_sp2[, 5],  result_sp2[, 6],  mu_true)
table_sp2[4, ] <- evaluation(result_sp2[, 7],  result_sp2[, 8],  mu_true)
table_sp2[5, ] <- evaluation(result_sp2[, 9],  result_sp2[, 10], mu_true)


########################################
# Simulation 3: sp1 + sp2 (multi-reference)
# sp1 covers x1, x2; sp2 covers x3, x4
########################################

result_both <- matrix(NA, nrow = n_sim, ncol = 4)
colnames(result_both) <- c("mean_precali_TRUE",  "se_precali_TRUE",
                           "mean_precali_FALSE", "se_precali_FALSE")

for (i in seq_len(n_sim)) {

  # nonprobability sample
  set.seed(seeds_sc[i])
  sc <- fp[rbinom(N, 1, pi_c) == 1,
           c("x1", "x2", "x3", "x4", "y", "wt_sc")]

  # sp1: Poisson, covers x1 and x2
  set.seed(seeds_sp[i])
  sp1 <- fp[rbinom(N, 1, pi_p1) == 1, c("x1", "x2", "wt_sp1")]
  sp1$pi_sp1 <- 1 / sp1$wt_sp1
  des_sp1 <- svydesign(ids = ~1, probs = ~pi_sp1, data = sp1)

  # sp2: systematic PPS, covers x3 and x4
  # sp2 continues from the same random stream as sp1 within each replication,
  # matching the paper's simulation design.
  s2  <- UPrandomsystematic(pi_p2)
  sp2 <- fp[s2 == 1, c("x3", "x4", "wt_sp2", "pi_sp2")]
  des_sp2 <- svydesign(ids = ~1, fpc = ~pi_sp2, data = sp2, pps = "brewer")

  # multi-reference with precalibration
  fit_true <- est_pw(
    data      = list(sc, des_sp1, des_sp2),
    precali   = TRUE,
    p_formula = list(~ x1 + x2, ~ x3 + x4)
  )
  res_true <- pwmean(fit_true, y = "y")

  # multi-reference without precalibration
  fit_false <- est_pw(
    data      = list(sc, des_sp1, des_sp2),
    precali   = FALSE,
    p_formula = list(~ x1 + x2, ~ x3 + x4)
  )
  res_false <- pwmean(fit_false, y = "y")

  result_both[i, 1:2] <- c(res_true$estimates$adjusted_mean,
                            res_true$estimates$adjusted_se)
  result_both[i, 3:4] <- c(res_false$estimates$adjusted_mean,
                            res_false$estimates$adjusted_se)

  if (i %% 100 == 0) cat(i, " ")
}

table_both <- matrix(NA, nrow = 2, ncol = 5)
colnames(table_both) <- c("RB(%)", "v(*100)", "MSE(*100)", "SER", "CP")
rownames(table_both) <- c("Multi (precali = TRUE)", "Multi (precali = FALSE)")

table_both[1, ] <- evaluation(result_both[, "mean_precali_TRUE"],
                              result_both[, "se_precali_TRUE"],  mu_true)
table_both[2, ] <- evaluation(result_both[, "mean_precali_FALSE"],
                              result_both[, "se_precali_FALSE"], mu_true)


########################################
# Results summary
########################################
# To export results, uncomment the write.csv() lines.

cat("\n\n--- Table 1, Case 0: one-reference (sp1, Poisson) ---\n")
print(kable(round(table_sp1, 2), align = "c"))

cat("\n--- Table 1, Case 0: one-reference (sp2, PPS) ---\n")
print(kable(round(table_sp2, 2), align = "c"))

cat("\n--- Table 2, Case PC.1: multi-reference (sp1 + sp2) ---\n")
print(kable(round(table_both, 2), align = "c"))

# write.csv(round(table_sp1,  2), "table_sp1.csv")
# write.csv(round(table_sp2,  2), "table_sp2.csv")
# write.csv(round(table_both, 2), "table_both.csv")


# Restore user's global options
options(old_options)
