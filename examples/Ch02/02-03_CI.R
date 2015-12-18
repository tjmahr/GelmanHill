library("ggplot2")

## CI for continuous data

# "For example, suppose an object is weighed five times, with measurements
# y = 35, 34, 38, 35, 37..." [18]
y <- c(35, 34, 38, 35, 37)
n <- length(y)
estimate <- mean(y)
se <- sd(y) / sqrt(n)
int_50 <- estimate + qt(c(0.25, 0.75), n - 1) * se
int_95 <- estimate + qt(c(0.025, 0.975), n - 1) * se


## CI for proportions

# "For example, if 700 persons in a random sample support the death penalty and
# 300 oppose it..." [18]
y <- 700
n <- 1000
estimate <- y / n
se <- sqrt(estimate * (1 - estimate) / n)
int_95 <- estimate + qnorm(c(0.025, 0.975)) * se


## CI for discrete data

# "For example, consider a hypothetical survey that asks 1000 randomly selected
# adults how many dogs they own, and suppose 600 have no dog, 300 have 1 dog, 50
# have 2 dogs, 30 have 3 dogs, and 20 have 4 dogs" [18]
y <- rep(c(0, 1, 2, 3, 4), c(600, 300, 50, 30, 20))
n <- length(y)
estimate <- mean(y)
se <- sd(y) / sqrt(n)
int_50 <- estimate + qt(c(0.25, 0.75), n - 1) * se
int_95 <- estimate + qt(c(0.025, 0.975), n - 1) * se


## Plot Figure 2.3

# Gallup polls on support for the death penalty
polls <- read.table("examples/Ch02/polls.dat")
colnames(polls) <- c("year", "month", "support", "oppose", "no_opinion")
head(polls)

support <- polls$support / (polls$support + polls$oppose)
year <- polls$year + (polls$month - 6) / 12

# Assumes n = 1000
y_se <- sqrt(support * (1 - support) / 1000)
y_max <- 100 * (support + y_se)
y_min <- 100 * (support - y_se)

frame1 <- data.frame(year = year, support = support * 100)
frame1$ymin <- y_min
frame1$ymax <- y_max

m <- ggplot(frame1) +
  aes(x = year, y = support, ymin = y_min, ymax = y_max) +
  geom_pointrange() +
  scale_x_continuous("Year") +
  scale_y_continuous("Percentage Support for the Death Penalty") +
  theme_bw()
m


## Weighted averages

# "For example, suppose that separate surveys conducted in France, Germany,
# Italy, and other countries yield estimates of 55% ± 2%, 61% ± 3%, 38% ± 3%,
# ..., for some opinion question. The estimated proportion for all adults in the
# European Union is..." [19]

surveys <- data.frame(
  country = c("France", "Germany", "Italy"),
  N = c(65633200, 80523700, 59685200),
  # estimated proportions of Yes responses
  p = c(0.55, 0.61, 0.38),
  se = c(0.02, 0.03, 0.03)
)

w_avg <- sum(surveys$N * surveys$p) / sum(surveys$N)
se_w_avg <- sqrt(sum((surveys$N * surveys$se / sum(surveys$N)) ^ 2))
int_95 <- w_avg + c(-2, 2) * se_w_avg


## CI using simulations

# "Now suppose these 1000 respondents include 500 men and 500 women, and suppose
# that the death penalty was supported by 75% of the men in the sample and only
# 65% of the women. We would like to estimate the ratio of support for the death
# penalty among men to that among women. The estimate is easily seen to be
# 0.75/0.65 = 1.15 — men support it 15%more than women—but computing the
# standard error is more challenging. The most direct approach, which we
# recommend, uses simulation." [20]

n_men <- 500
p_hat_men <- 0.75
se_men <- sqrt(p_hat_men * (1 - p_hat_men) / n_men)

n_women <- 500
p_hat_women <- 0.65
se_women <- sqrt(p_hat_women * (1 - p_hat_women) / n_women)

mean_ratio <- p_hat_men / p_hat_women

# randomly draw proportions for men and women
n_sims <- 10000
p_men <- rnorm(n_sims, p_hat_men, se_men)
p_women <- rnorm(n_sims, p_hat_women, se_women)

# then measure the interval on the intervals in the simulated data
ratio <- p_men / p_women
int_95 <- quantile(ratio, c(0.025, 0.975))
int_95
