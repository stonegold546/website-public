library(lavaan)
library(lme4)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

cfa.mm <- stan_model(stanc_ret = stanc(file = "cfa.stan"))

dat <- HolzingerSwineford1939
dat$sex <- dat$sex - 1 # Who is 1, no idea
dat$grade <- dat$grade - min(dat$grade, na.rm = TRUE)
dat$ID <- 1:nrow(dat)
dat.l <- tidyr::gather(dat, item, score, x1:x9)
dat.l$item.no <- as.integer(gsub("x", "", dat.l$item))

# ------ Basic random intercept model

lmer(score ~ 0 + factor(item.no) + (1 | ID), dat.l, REML = FALSE)

parameterEstimates(sem(
  "F1 =~ a * x1 + a * x2 + a * x3 + a * x4 + a * x5 + a * x6 + a * x7 + a * x8 + a * x9\n
  x1 ~~ f * x1\nx2 ~~ f * x2\nx3 ~~ f * x3\nx4 ~~ f * x4\n
  x5 ~~ f * x5\nx6 ~~ f * x6\nx7 ~~ f * x7\nx8 ~~ f * x8\nx9 ~~ f * x9",
  dat, std.lv = TRUE
), standardized = TRUE)[c(1:2, 10:11), c(1:5, 12)]

# ------ Random slopes model

dat.l$Fs <- ((dat.l$item.no - 1) %/% 3) + 1

lmer(score ~ 0 + factor(item) + (0 + factor(Fs) | ID), dat.l, REML = FALSE)

parameterEstimates(sem(
  "F1 =~ a * x1 + a * x2 + a * x3\nF2 =~ b * x4 + b * x5 + b * x6\nF3 =~ c * x7 + c * x8 + c * x9\n
  x1 ~~ f * x1\nx2 ~~ f * x2\nx3 ~~ f * x3\nx4 ~~ f * x4\nx5 ~~ f * x5\n
  x6 ~~ f * x6\nx7 ~~ f * x7\nx8 ~~ f * x8\nx9 ~~ f * x9",
  dat, std.lv = TRUE
), standardized = TRUE)[c(1:10, 22:24), c(1:5, 12)]

# ------ Basic CFA

cfa.lav.fit <- sem(
  "F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nF3 =~ x7 + x8 + x9",
  dat, std.lv = TRUE, meanstructure = TRUE)

cfa.stan.fit <- sampling(
  cfa.mm, data = list(
    N = nrow(dat.l), Ni = length(unique(dat.l$item)),
    Np = length(unique(dat.l$ID)), Nf = length(unique(dat.l$Fs)),
    items = dat.l$item.no, factors = dat.l$Fs,
    persons = dat.l$ID, response = dat.l$score,
    g_alpha = 1, g_beta = 1),
  control = list(adapt_delta = .99999, max_treedepth = 15))

print(cfa.stan.fit, pars = c("alphas", "loadings_std"),
      probs = c(.025, .5, .975), digits_summary = 3)

parameterEstimates(cfa.lav.fit, standardized = TRUE)[1:9, c(1:5, 11)]

print(cfa.stan.fit, pars = c("R[1, 2]", "R[1, 3]", "R[2, 3]"),
      probs = c(.025, .5, .975), digits_summary = 3)

parameterEstimates(cfa.lav.fit, standardized = TRUE)[22:24, c(1:5, 11)]

print(cfa.stan.fit, pars = c("item_vars"),
      probs = c(.025, .5, .975), digits_summary = 3)

parameterEstimates(cfa.lav.fit, standardized = TRUE)[10:18, 1:5]

print(cfa.stan.fit, pars = c("betas"),
      probs = c(.025, .5, .975), digits_summary = 3)

parameterEstimates(cfa.lav.fit, standardized = TRUE)[25:33, 1:5]

# ------ Assuming intending to regress factor 1 on both factors 2 and 3

R <- extract(cfa.stan.fit, c("R[1, 2]", "R[1, 3]", "R[2, 3]"))
R <- cbind(R$`R[1,2]`, R$`R[1,3]`, R$`R[2,3]`)
coefs <- matrix(NA, nrow(R), ncol(R) - 1)
for (i in 1:nrow(R)) {
  m <- matrix(c(1, R[i, 3], R[i, 3], 1), 2, 2)
  coefs[i, ] <- solve(m, R[i, 1:2])
}; rm(i, m)
t(apply(coefs, 2, function (x) {
  c(estimate = mean(x), sd = sd(x), quantile(x, c(.025, .25, .5, .75, .975)))
}))

