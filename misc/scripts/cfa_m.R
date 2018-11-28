library(lavaan)

set.seed(12345)
cfa.dat <- simulateData(
  model = "F1 =~ X1 + .8 * X2 + .75 * X3\nF2 =~ .5 * X4 + .75 * X5\n
  F3 =~ .65 * X6 + .85 * X7 + .45 * X8\nF1 ~~ .5 * F2\nF1 ~~ .5 * F3\nF2 ~~ .5 * F3\n
  X1 ~ 1.5 * 1\nX3 ~ -1.5 * 1\nX5 ~ -.5 * 1\nX7 ~ .5 * 1\n
  X1 ~~ 1.5 * X1\nX2 ~~ 0.5 * X2\nX3 ~~ 0.75 * X3\nX4 ~~ 2 * X4\n
  X5 ~~ 1 * X5\nX6 ~~ 1.25 * X6\nX7 ~~ 1.75 * X7\nX8 ~~ 1 * X8",
  sample.nobs = 150, meanstructure = TRUE
)

library(lme4)

# Make data long
cfa.dat$ID <- 1:nrow(cfa.dat)
cfa.dat.l <- tidyr::gather(cfa.dat, item, response, X1:X8)
cfa.dat.l$item <- as.integer(substr(cfa.dat.l$item, 2, 2))

lmer(response ~ 0 + factor(item) + (1 | ID), cfa.dat.l, REML = FALSE)

parameterEstimates(sem(
  "F1 =~ a * X1 + a * X2 + a * X3 + a * X4 + a * X5 + a * X6 + a * X7 + a * X8\n
  X1 ~~ f * X1\nX2 ~~ f * X2\nX3 ~~ f * X3\nX4 ~~ f * X4\n
  X5 ~~ f * X5\nX6 ~~ f * X6\nX7 ~~ f * X7\nX8 ~~ f * X8",
  cfa.dat, std.lv = TRUE
), standardized = TRUE)[c(1:2, 9:10), c(1:5, 12)]

cfa.dat.l$factor <- ifelse(cfa.dat.l$item %in% 1:3, 1, ifelse(cfa.dat.l$item %in% 4:5, 2, 3))

lmer(response ~ 0 + factor(item) + (0 + factor(factor) | ID), cfa.dat.l, REML = FALSE)

parameterEstimates(sem(
  "F1 =~ a * X1 + a * X2 + a * X3\nF2 =~ b * X4 + b * X5\nF3 =~ c * X6 + c * X7 + c * X8\n
  X1 ~~ f * X1\nX2 ~~ f * X2\nX3 ~~ f * X3\nX4 ~~ f * X4\n
  X5 ~~ f * X5\nX6 ~~ f * X6\nX7 ~~ f * X7\nX8 ~~ f * X8",
  cfa.dat, std.lv = TRUE
), standardized = TRUE)[c(1:10, 20:22), c(1:5, 12)]

"F1 =~ a * X1 + a * X2 + a * X3\nF2 =~ b * X4 + b * X5\nF3 =~ c * X6 + c * X7 + c * X8"

library(rstan)

options(mc.cores = parallel::detectCores()) # Use multiple cores
rstan_options(auto_write = TRUE) # One time Stan program compilation

# Compile Stan code
cfa.mm <- stan_model(stanc_ret = stanc(file = "cfa_m.stan"))

cfa.stan.fit <- sampling(
  cfa.mm, data = list(
    N = nrow(cfa.dat.l), Ni = length(unique(cfa.dat.l$item)),
    Np = length(unique(cfa.dat.l$ID)), Nf = length(unique(cfa.dat.l$factor)),
    items = as.numeric(cfa.dat.l$item), factors = as.numeric(cfa.dat.l$factor),
    persons = as.numeric(cfa.dat.l$ID), response = cfa.dat.l$response),
  control = list(adapt_delta = .99999, max_treedepth = 15))

# Relevant CFA parameters
print(cfa.stan.fit, pars = c(
  "alphas", "loadings_std", "item_vars", "betas",
  "R[1, 2]", "R[1, 3]", "R[2, 3]"),
  probs = c(.025, .5, .975), digits_summary = 3)
