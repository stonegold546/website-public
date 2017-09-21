library(BaylorEdPsych)
library(simglm)
library(ggplot2)
library(dplyr)
library(mice)
fixed <- ~1 + age + income
fixed_param <- c(2, 0.3, 1.3)
cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                  var_type = c("single", "single"),
                  opts = list(list(mean = 0, sd = 4),
                              list(mean = 0, sd = 3)))
n <- 150
error_var <- 3
with_err_gen <- 'rnorm'
temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                       cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen,
                       data_str = "single")

psych::describe(temp_single)
lm(sim_data ~ age + income, temp_single)

tmp_single_miss <- missing_data(temp_single, miss_prop = .25, 
                                type = 'random', clust_var = NULL)
arm::display(lm(sim_data ~ age + income, tmp_single_miss))
arm::display(lm(sim_data2 ~ age + income, tmp_single_miss))
LittleMCAR(tmp_single_miss[c("age", "income", "sim_data2")])$p.value

nRep <- 2000
n.s <- seq(50, 1500, 150)
little.mcar.p <- matrix(ncol = 2, nrow = nRep*length(n.s))
samp.dat <- sample(1:nRep*length(n.s), 50)
dat.s <- list()
for(i in 1:nRep) {
  for(j in 1:length(n.s)) {
    er <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                  cov_param = cov_param,
                  n = n.s[j], error_var = error_var, with_err_gen = with_err_gen,
                  data_str = "single")
    er <- as.data.frame(
      ampute(er[c("age", "income", "sim_data")], mech = "MCAR")$amp)
    iter.n <- i * length(n.s) - length(n.s) + j
    if(iter.n %in% samp.dat) {
      dat.s <- c(dat.s, list(er))
    }
    little.mcar.p[iter.n, 1] <- n.s[j]
    little.mcar.p[iter.n, 2] <- 
      # TestMCARNormality(er)$pnormality
      LittleMCAR(er)$p.value
    rm(er)
  }
}
little.mcar.p <- as.data.frame(little.mcar.p)
names(little.mcar.p) <- c("n", "p")
little.mcar.p$n <- factor(little.mcar.p$n, ordered = TRUE, levels = n.s)

ggplot(little.mcar.p, aes(x = n, y = p)) + geom_boxplot() +
  geom_crossbar(aes(ymin = q025, y = q05, ymax = q075), data = summarise(
    group_by(little.mcar.p, n), q025 = quantile(p, .025, na.rm = TRUE),
    q05 = quantile(p, .05, na.rm = TRUE), q075 = quantile(p, .075, na.rm = TRUE)
  )) +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Sample size", y = "p-value",
       title = "Little's MCAR test for data that are MCAR",
       subtitle = "2000 replications",
       caption = paste(paste("For the narrow boxes, going from top to bottom, lines",
                             "represent 7.5th, 5th and 2.5th percentiles of p-values."),
                       "Test maintains nominal error rate across wide range of sample sizes.",
                       sep = "\n"))
ggsave("./zoom_out.png")
ggplot(little.mcar.p, aes(x = n, y = p)) + # geom_boxplot() +
  geom_crossbar(aes(ymin = q025, y = q05, ymax = q075), data = summarise(
    group_by(little.mcar.p, n), q025 = quantile(p, .025, na.rm = TRUE),
    q05 = quantile(p, .05, na.rm = TRUE), q075 = quantile(p, .075, na.rm = TRUE)
  )) +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .01), limits = c(0, .1)) +
  labs(x = "Sample size", y = "p-value",
       title = "Little's MCAR test for data that are MCAR",
       subtitle = "2000 replications",
       caption = paste(paste("Going from top to bottom, lines for each sample size represent",
                             "7.5th, 5th and 2.5th percentiles of p-values."),
                       "Test maintains nominal error rate across wide range of sample sizes.",
                       sep = "\n"))
ggsave("./zoom_in.png")

# MAR
nRep <- 2000
n.s.mar <- seq(40, 220, 20)
little.mcar.p.mar <- matrix(ncol = 2, nrow = nRep*length(n.s.mar))
samp.dat.mar <- sample(1:nRep*length(n.s.mar), 50)
dat.s.mar <- list()
for(i in 1:nRep) {
  for(j in 1:length(n.s.mar)) {
    er <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                  cov_param = cov_param,
                  n = n.s.mar[j], error_var = error_var, with_err_gen = with_err_gen,
                  data_str = "single")
    er <- as.data.frame(
      ampute(er[c("age", "income", "sim_data")], mech = "MAR")$amp)
    iter.n <- i * length(n.s.mar) - length(n.s.mar) + j
    if(iter.n %in% samp.dat.mar) {
      dat.s.mar <- c(dat.s.mar, list(er))
    }
    little.mcar.p.mar[iter.n, 1] <- n.s.mar[j]
    little.mcar.p.mar[iter.n, 2] <- 
      LittleMCAR(er)$p.value
    rm(er)
  }
}
little.mcar.p.mar <- as.data.frame(little.mcar.p.mar)
names(little.mcar.p.mar) <- c("n", "p")
little.mcar.p.mar$n <- factor(little.mcar.p.mar$n, ordered = TRUE, levels = n.s.mar)

ggplot(little.mcar.p.mar, aes(x = n, y = p)) + geom_boxplot() +
  geom_crossbar(aes(ymin = q925, y = q95, ymax = q975), data = summarise(
    group_by(little.mcar.p.mar, n), q925 = quantile(p, .925, na.rm = TRUE),
    q95 = quantile(p, .95, na.rm = TRUE), q975 = quantile(p, .975, na.rm = TRUE)
  ), linetype = 2) +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Sample size", y = "p-value",
       title = "Little's MCAR test for data that are MAR",
       subtitle = "2000 replications",
       caption = paste(paste("For the dashed boxes, going from top to bottom, lines",
                             "represent 97.5th, 95th and 92.5th percentiles of p-values."),
                       "Test only maintains nominal error rate around sample size of 120.",
                       sep = "\n"))
ggsave("./zoom_out_mar.png")
ggplot(little.mcar.p.mar, aes(x = n, y = p)) + # geom_boxplot() +
  geom_crossbar(aes(ymin = q925, y = q95, ymax = q975), data = summarise(
    group_by(little.mcar.p.mar, n), q925 = quantile(p, .925, na.rm = TRUE),
    q95 = quantile(p, .95, na.rm = TRUE), q975 = quantile(p, .975, na.rm = TRUE)
  )) +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, .8)) +
  labs(x = "Sample size", y = "p-value",
       title = "Little's MCAR test for data that are MAR",
       subtitle = "2000 replications",
       caption = paste(paste("Going from top to bottom, lines for each sample size",
                             "represent 97.5th, 95th and 92.5th percentiles of p-values."),
                       "Test only maintains nominal error rate around sample size of 120.",
                       sep = "\n"))
ggsave("./zoom_in_mar.png")
