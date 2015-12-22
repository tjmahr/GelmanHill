#' # 07-02 Summarizing Linear Regression Using Simulation

#+ setup, include = FALSE
library("knitr")
opts_chunk$set(collapse = TRUE, comment = "#>", fig.path = "figure/07-02/")
opts_knit$set(root.dir = "../../")

#+ warning = FALSE, message = FALSE
library("ggplot2")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Read in the objects from the dump-file and store in list
earnings_env <- new.env()
source("examples/Ch07/earnings.data.R", local = earnings_env)
earnings <- as.list(earnings_env)

# Compute transformed/derived values
earnings$log_earn <- log(earnings$earnings)
earnings$male <- 2 - earnings$sex1
str(earnings)


#' ## Simulation to represent predictive uncertainty
#'
#' ### Model of log earnings with interactions


# Fit model
m_earn3 <- lm(log_earn ~ height + male + height:male, earnings)
summary(m_earn3)

#' For direct prediction from an R model, use `predict`. Here we predict the
#' earnings of a 68-inch tall man.
x_new <- data.frame(height = 68, male = 1)
pred_interval <- predict(m_earn3, x_new, interval = "prediction", level = .95)
pred_interval

# On the original scale
exp(pred_interval)



#' ## Constructing the predictive interval using simulation
#'
#' Draw from normal distributions where the means are the estimated heights for
#' 68-inch tall men and women and where the standard deviation is the residual
#' standard error in the model.
#'
#' Note that these draws ignore uncertainty in the estimates of the regression
#' parameters.

# Set mean and sd
m_68 <- predict(m_earn3, data.frame(male = 1, height = 68))
f_68 <- predict(m_earn3, data.frame(male = 0, height = 68))
sigma_y <- summary(m_earn3)$sigma

# Perform draws
pred_man <- exp(rnorm(1000, m_68, sigma_y))
pred_woman <- exp(rnorm(1000, f_68, sigma_y))

#' Compare the simulated interval to the one given by `predict`:
quantile(pred_man, probs = c(.025, .975))

#' Now we can make predictive intervals for more complicated statistics, such as
#' the difference on the original dollar scale.
pred_diff <- pred_man - pred_woman
summary(pred_diff)
quantile(pred_diff, probs = c(.025, .975))

pred_ratio <- pred_man / pred_woman
summary(pred_ratio)
quantile(pred_ratio, probs = c(.025, .975))



#' ### Histograms (Figure 07-02)

log_pred_man <- rnorm(1000, m_68, sigma_y)
frame1 <- data.frame(x1 = log_pred_man)

#+ message = FALSE
p1 <- ggplot(frame1) +
  aes(x = x1) +
  geom_histogram(colour = "black", fill = "white") +
  xlab("log(earnings)") +
  theme_bw()
p1

p2 <- ggplot(frame1) +
  aes(x = exp(x1)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10000) +
  scale_x_continuous(labels = scales::dollar) +
  xlab("earnings") +
  theme_bw()
p2



#' ## Simulations that account for parameter uncertainty
#'
#' Use `arm::sim`. The procedure uses classical regression to get the estimated
#' parameters (beta-hats), a covariance matrix V_beta, and the residual
#' variance. It then simulates sigmas and betas from those values. See p. 143.

simulated_parameters <- arm::sim(m_earn3, 1000)
head(coef(simulated_parameters))

height_beta <- coef(simulated_parameters)[, "height"]
mean(height_beta)
sd(height_beta)
quantile(height_beta, c(.025, .975))

#' We can compute estimates of the male slope (the sum of the height parameter
#' and the male:height parameters) using simulation.
height_for_men <- height_beta + coef(simulated_parameters)[, "height:male"]
quantile(height_for_men, c(.025, .975))



#' ## Stan version of the procedure

#' Fit the model in Stan:

dl_3 <- list(
  N = earnings$N,
  earnings = earnings$earnings,
  height = earnings$height,
  sex = earnings$sex1
)

earnings_interactions_sf1 <- stan(
  file = "examples/Ch07/earnings_interactions.stan",
  data = dl_3,
  iter = 1000,
  chains = 4
)

#' Review the model's syntax:
earnings_interactions_sf1@stanmodel

#' Inference summary:
earnings_interactions_sf1

#' Use the posterior samples from the model as the simulated values.
post <- extract(earnings_interactions_sf1)

height_coef <- post$beta[, 2]
mean(height_coef)
sd(height_coef)
quantile(height_coef, c(.025, .975))

height_for_men_coef <- post$beta[, 2] + post$beta[, 4]
quantile(height_for_men_coef, c(.025, .975))
