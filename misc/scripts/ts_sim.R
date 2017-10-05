library(simglm)
library(ggplot2)
library(dplyr)
library(WRS)

# Hetero
nRep <- 100
n.s <- c(seq(50, 300, 50), 400, 550, 750, 1000)
samp.dat <- sample((1:(nRep*length(n.s))), 25)
lm.coefs.0 <- matrix(ncol = 3, nrow = nRep*length(n.s))
ts.coefs.0 <- matrix(ncol = 3, nrow = nRep*length(n.s))
lmt.coefs.0 <- matrix(ncol = 3, nrow = nRep*length(n.s))
dat.s <- list()
fixed <- ~ 1 + age
fixed_param <- c(2, 2)
cov_param <- list(dist_fun = c("rnorm"),
                  var_type = c("single"),
                  opts = list(list(mean = 0, sd = 4)))
# (error_var <- sort(c(sample(3:10, 1), sample(30:50, 1), sample(75:150, 1)),
#                    decreasing = as.logical(sample(0:1, 1))))
with_err_gen <- "rnorm"
for(i in 1:nRep) {
  for(j in 1:length(n.s)) {
    error_var <- sort(c(sample(3:10, 1), sample(30:50, 1), sample(75:150, 1)),
                      decreasing = as.logical(sample(0:1, 1)))
    er <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                  cov_param = cov_param,
                  n = n.s[j], error_var = error_var, with_err_gen = with_err_gen,
                  data_str = "single",
                  homogeneity = FALSE, heterogeneity_var = "age")
    x <- er$age
    y <- er$sim_data
    iter.n <- i * length(n.s) - length(n.s) + j
    lm.coefs.0[iter.n, ] <- c(n.s[j], coef(lm(y ~ x)))
    lmt.coefs.0[iter.n, ] <- c(n.s[j], coef(heavyLm(y ~ x)))
    ts.coefs.0[iter.n, ] <- c(n.s[j], tsreg(x, y, iter = 1)$coef)
    if(iter.n %in% samp.dat) {
      dat.s <- c(dat.s, list(er))
    }
    rm(er, x, y)
  }
}
coefs.0 <- as.data.frame(rbind(lm.coefs.0, ts.coefs.0, lmt.coefs.0))
coefs.0$V4 <- c(rep("OLS", nRep*length(n.s)), rep("Theil-Sen", nRep*length(n.s)),
                rep("t-distribution", nRep*length(n.s)))
names(coefs.0) <- c("n", "Intercept", "Slope", "Estimator")
coefs.0$n <- factor(coefs.0$n, ordered = TRUE, levels = n.s)
dat.frms.0 <- data.frame()
for(k in 1:length(dat.s)) {
  dat.frms.0 <- rbind(dat.frms.0, cbind(dat.s[[k]], rep(k, nrow(dat.s[[k]]))))
}
rm(lm.coefs.0, ts.coefs.0, dat.s)
names(dat.frms.0)[7] <- "random.sample"
dat.frms.0$random.sample <- factor(dat.frms.0$random.sample,
                                   levels = 1:length(samp.dat))

ggplot(dat.frms.0, aes(x = age, y = sim_data)) +
  geom_point(shape = 1, size = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ random.sample, nrow = 5) +
  labs(x = "Predictor", y = "Outcome",
       title = "Random sample of 25 datasets from 15000 datasets for simulation",
       subtitle = "Heteroscedastic relationships")
ggsave("./0_heteroscedastic_samples.png")

ggplot(coefs.0, aes(x = n, colour = Estimator)) +
  geom_boxplot(
    aes(ymin = q025, lower = q25, middle = q50, upper = q75, ymax = q975), data = summarise(
      group_by(coefs.0, n, Estimator), q025 = quantile(Slope, .025),
      q25 = quantile(Slope, .25), q50 = quantile(Slope, .5),
      q75 = quantile(Slope, .75), q975 = quantile(Slope, .975)), stat = "identity") +
  geom_hline(yintercept = 2, linetype = 2) + scale_y_continuous(breaks = seq(1, 3, .05)) +
  labs(x = "Sample size", y = "Slope",
       title = "Estimation of regression slope in simple linear regression under heteroscedasticity",
       subtitle = "1500 replications - Population slope is 2",
       caption = paste(
         "Boxes are IQR, whiskers are middle 95% of slopes",
         "Both estimators are unbiased in the long run, however, OLS has higher variability",
         sep = "\n"
       ))
ggsave("./0_slopes_hetero.png")
