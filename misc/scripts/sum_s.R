library(lavaan)
library(psych)

sim.fun <- function (lv, lambda, nrep = 2e3) {
  np <- nrow(lv)
  t(replicate(nrep, {
    X <- lv %*% lambda +
      matrix(rnorm(np * length(lambda), 0, sqrt(1 - lambda ^ 2)), np, byrow = TRUE)
    # summary(cfa(paste("F =~", paste0("V", 1:length(lambda), collapse = " + ")), X, std.lv = TRUE))
    
    ss <- rowSums(X)
    fs <- predict(cfa(paste("F =~", paste0("V", 1:length(lambda), collapse = " + ")), X))
    
    c(alpha = psych::alpha(X)$total$raw_alpha, ss = cor(lv, ss), fs = cor(lv, fs))
  }))
}

# c(.7, .8, .6, .7, .75, .5, .85, .55, .65)
np <- 6e1
lat <- matrix(qnorm((1:np - .5) / np))

(loadings.1 <- seq(.9, .6, length.out = 9)) ^ 2
res.1 <- sim.fun(lat, loadings.1)
colMeans(res.1)
apply(res.1, 2, function (x) mean((x - 1) ^ 2))

(loadings.2 <- seq(.9, .6, length.out = 3)) ^ 2
res.2 <- sim.fun(lat, loadings.2)
colMeans(res.2)
apply(res.2, 2, function (x) mean((x - 1) ^ 2))

(loadings.3 <- seq(.5, .3, length.out = 9)) ^ 2
res.3 <- sim.fun(lat, loadings.3)
colMeans(res.3)
apply(res.3, 2, function (x) mean((x - 1) ^ 2))

(loadings.4 <- seq(.5, .3, length.out = 3)) ^ 2
res.4 <- sim.fun(lat, loadings.4)
colMeans(res.4)
apply(res.4, 2, function (x) mean((x - 1) ^ 2))

(loadings.5 <- seq(.9, .3, length.out = 9)) ^ 2
res.5 <- sim.fun(lat, loadings.5)
colMeans(res.5)
apply(res.5, 2, function (x) mean((x - 1) ^ 2))

library(dplyr)
library(ggplot2)
library(scales)
library(directlabels)

theme_set(theme_classic())

res.all <- bind_rows(lapply(list(res.1, res.2, res.3, res.4, res.5), as.data.frame), .id = "design") %>%
  mutate(design = case_when(
    design == 1 ~ "9 items strong",
    design == 2 ~ "3 items strong",
    design == 3 ~ "9 items weak",
    design == 4 ~ "3 items weak",
    design == 5 ~ "9 items hetero"
  ), design = factor(design, levels = c("3 items weak", "9 items weak", "3 items strong",
                                        "9 items strong", "9 items hetero")))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

res.all %>%
  reshape(direction = "long", varying = 3:4, times = c("ss", "fs"), v.names = "correlation") %>%
  ggplot(aes(abs(correlation), fill = time)) + geom_density(alpha = .5) +
  geom_rug(aes(col = time), alpha = .25) +
  facet_wrap(~ design, scales = "free") +
  scale_fill_manual(values = cbbPalette, labels = c("Factor score", "Sum score")) +
  scale_color_manual(values = cbbPalette, labels = c("Factor score", "Sum score")) +
  theme(legend.position = c(.8, .3)) +
  labs(x = "", fill = "Approach", col = "Approach",
       subtitle = "Distribution of correlation to latent variable",
       caption = "The axes are different for each panel")
ggsave("cor.pdf", height = 5, width = 7)
ggsave("cor.png", height = 5, width = 7)
(tmp <- res.all %>%
    group_by(design) %>%
    summarise(alpha = mean(alpha), ss = mean((ss - 1) ^ 2),
              fs = mean((abs(fs) - 1) ^ 2),
              ratio = number(fs / ss, .01)))
tmp %>%
  ggplot(aes(ss, fs)) + geom_point(aes(size = ratio), shape = 1) + coord_fixed() +
  geom_dl(aes(label = design), method = "smart.grid") +
  scale_y_continuous(trans = log_trans(), breaks = round(tmp$fs, 4)) +
  scale_x_continuous(trans = log_trans(), breaks = round(tmp$ss, 4)) +
  scale_size_discrete(guide = guide_legend(reverse = TRUE)) +
  theme(legend.box.just = 0:1, legend.position = c(.25, .75)) +
  labs(x = "Sum scores", y = "Factor scores", size = "Factor MSE / Sum MSE",
       subtitle = "Mean squared error of correlation to latent variable",
       caption = "Axes are on natural log scale")
ggsave("mse.pdf", height = 5.5, width = 5)
ggsave("mse.png", height = 5.5, width = 5)

xlmat <- data.frame(loadings = c(loadings.1, loadings.2, loadings.3, loadings.4, loadings.5),
                   design = c(rep("9 items strong", 9), rep("3 items strong", 3), rep("9 items weak", 9),
                              rep("3 items weak", 3), rep("9 items hetero", 9))) %>%
  mutate(design = factor(design, levels = rev(c("9 items strong", "3 items strong", "9 items weak",
                                                "3 items weak", "9 items hetero"))))

lmat %>%
  ggplot(aes(design, loadings)) + geom_point(shape = 1) + coord_flip() +
  scale_y_continuous(breaks = seq(.3, .9, .05),
                     sec.axis = sec_axis(trans = ~ . ^ 2, breaks = seq(.3, .9, .075) ^ 2,
                                         labels = percent_format(), name = "R-square")) +
  labs(x = "Loading set", y = "Loadings")
ggsave("loadings.pdf", height = 3, width = 7)
ggsave("loadings.png", height = 3, width = 7)
