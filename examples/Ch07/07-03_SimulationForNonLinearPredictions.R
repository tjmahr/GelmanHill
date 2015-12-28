#' # 07-03 Simulation for Nonlinear Predictions

#+ setup, include = FALSE
library("knitr")
opts_chunk$set(collapse = TRUE, comment = "#>", fig.path = "figure/07-03/")
opts_knit$set(root.dir = "../../")
options(width = 80)

#+ warning = FALSE, message = FALSE
library("ggplot2")
library("dplyr")
library("stringr")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' We're going to fit a model that predicts that Democratic share of the
#' two-party vote in congressional elections. First, we predict 1988 results
#' using the 1986 results and incumbency (i.e., whether a Democrat or Republican
#' were up for reelection or whether the seat was open). Elections with less 10%
#' or more than 90% Democratic votes are "uncontested". Then, using the
#' parameters estimated by this model, we're going to predict the outcomes of
#' the 1990 elections using simulations.

# These csvs were generated using code in 07-00_CleaningElectionData.R
df_cong <- read.csv("examples/Ch07/congress_long.csv") %>% as.tbl
df_cong

# Same data from above but spread out by year, so we can do things like use 1986
# columns to predict 1988 columns.
df_cong_wide <- read.csv("examples/Ch07/congress_wide.csv") %>% as.tbl
str(df_cong_wide)


#' ## Figure 7.3
v88_hist <- df_cong %>%
  filter(year == 1988) %>%
  # Turn uncontested proportions into landslides, for presentation purposes
  mutate(Landslides = ifelse(DemShare < 0.1, .0001, DemShare),
         Landslides = ifelse(0.9 < DemShare, 0.9999, Landslides))

#+ warning = FALSE
p1 <- ggplot(v88_hist) +
  aes(x = Landslides) +
  geom_histogram(colour = "black", fill = "white", binwidth = 0.05) +
  scale_x_continuous("Democratic Share of the Two-Party Vote", limits = c(0, 1)) +
  labs(title = "Congressional Elections in 1988", y = "N Elections") +
  theme_bw()
p1
#' > Figure 7.3 _Histogram of congressional election data from 1988. The spikes
#' > at the left and right ends represent uncontested Republicans and Democrats,
#' > respectively._
#'
#'
#'
#' ## Fitting the model (R)
#'
#' We are modeling just the contested elections from 1988. That is, we are
#' excluded the spikes from the histogram above.
df_m <- df_cong_wide %>%
  filter(Contested_1988 == 1)

#' In the book, the model is fit with `lm(vote.88 ~ vote.86 + incumbency.88)`.
m_88 <- lm(DemShare_Adj_1988 ~ DemShare_Adj_1986 + Incumbent_1988, df_m)
summary(m_88)


#' ### Figure 7.4
#'
#' These plots show the raw data versus the modeled dataset.

# Create a dataframe just for plotting with shorter column names.
df_plot <- df_cong_wide
names(df_plot) <- names(df_plot) %>%
  str_replace("Contested", "Con") %>%
  str_replace("DemShare_", "Dem_") %>%
  str_replace("Incumbent", "Inc")

set_incumbent_factor <- function(xs) {
  incumbent_levels <- c(Republican = -1, Open = 0, Democrat = 1)
  factor(xs, incumbent_levels, names(incumbent_levels))
}

# Jitter uncontested points and create labels for incumbent status
df_plot <- df_plot %>%
  mutate(Jit_86 = ifelse(Con_1986, Dem_1986, jitter(Dem_1986, amount = 0.02)),
         Jit_88 = ifelse(Con_1988, Dem_1988, jitter(Dem_1988, amount = 0.02)),
         Inc_1988 = set_incumbent_factor(Inc_1988))

#+ warning = FALSE
p2 <- ggplot(df_plot) +
  aes(x = Jit_86, y = Jit_88, shape = Inc_1988) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_shape_manual(name = "Incumbent Party (1988)", values = c(4, 1, 16)) +
  xlab("Democratic Vote Share in 1986") +
  ylab("Democratic Vote Share in 1988") +
  labs(title = "Raw Data (jittered at 0 and 1)") +
  theme_bw() +
  theme(legend.position = "bottom")
p2

#' > Figure 7.4 (a) _Congressional election data from 1986 and 1988. Crosses
#' > correspond to elections with Republican incumbents running in 1988, dots
#' > correspond to Democratic incumbents, and open circles correspond to open
#' > seats. The “incumbency” predictor in the regression model equals 0 for the
#' > circles, +1 for the dots, and −1 for the crosses. Uncontested election
#' > outcomes (at 0 and 1) have been jittered slightly._
#'
#' The uncontested seats below line are the ones that get imputed to .25 or .75
#' for the model. We see these points in the next plot as a vertical band of
#' points at .25 and .75.
#'
#' The uncontested seats above the line are excluded from the model.

#+ warning = FALSE
p3 <- ggplot(df_m) +
  aes(x = DemShare_Adj_1986, DemShare_Adj_1988,
      shape = set_incumbent_factor(Incumbent_1988)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_shape_manual(name = "Incumbent Party (1988)", values = c(4, 1, 16)) +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  xlab("Democratic Vote Share in 1986") +
  ylab("Democratic Vote Share in 1988") +
  labs(title = "Adjusted data (imputing 0's to .25 and 1's to .75)") +
  theme_bw() +
  theme(legend.position = "bottom")
p3

#' > Figure 7.4 (b) _Data for the regression analysis, with uncontested 1988
#' > elections removed and uncontested 1986 election values replaced by 0.25 and
#' > 0.75. The y = x line is included as a comparison on both plots._
#'
#'
#'
#' ## Simulation for inferences and predictions of new data points (R)
#'
#' First, we create the predictor matrix, x-tilde.
x_tilde <- df_cong_wide %>%
  mutate(Constant = 1) %>%
  select(Constant, DemShare_1988, Incumbent_1990) %>%
  na.omit %>%
  as.matrix
tail(x_tilde)

#' Next, write a function to simulate one election. First, we draw parameter
#' estimates and model error. Then we multiply the predictor matrix by the
#' parameter estimates and add the model error.
predict_88 <- function(pred_matrix, model) {
  n_pred <- nrow(pred_matrix)
  sim_88 <- arm::sim(model, 1)
  # "For each simulation, we compute the predicted value Xtilde*β and add normal
  # errors." [146]
  y_pred <- rnorm(n_pred, pred_matrix %*% t(sim_88@coef), sim_88@sigma)
  y_pred
}

#' Finally, we simulate 1000 elections and count the number of Democrats
#' projected in each election.
y_tilde <- replicate(1000, predict_88(x_tilde, m_88))
n_democrats_tilde <- colSums(.5 < y_tilde, na.rm = TRUE)

p4 <- ggplot() +
  aes(x = n_democrats_tilde) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab("Democrats Elected (Out of 416 races)") +
  ylab("N Simulations (Out of 1000 simulations") +
  theme_bw()
p4

#' The book notes that the elections have a predictive uncertainty of
#' approximately .7. We can get this estimate by simulating sigma.
sigmas <- arm::sim(m_88, 1000)@sigma
summary(sigmas)

#' Our estimates our slightly off from the book, which was median = 253, mean =
#' 252.4 and sd = 3.1. (I'm not sure if this is a bug in my part, a bug in the
#' book, a bug in `arm` that has been resolved, or if my code and the book are
#' working on different subsets of data. It doesn't really matter.)
summary(n_democrats_tilde)
sd(n_democrats_tilde)

#' But the larger point remains:
#'
#' > "This estimate and standard error _could not_ simply be calculated from the
#' > estimates and uncertainties for the individual districts. Simulation is the
#' > only practical method of assessing the predictive uncertainty for this
#' > nonlinear function of the predicted outcomes." [147]
#'
#'
#'
#' ## Fitting the model (Stan)
#'
#' Use the same data as the R version, but first remove NA rows (list-wise
#' deletion).
df_stan <- df_m %>%
  select(DemShare_Adj_1986, Incumbent_1988, DemShare_1988) %>%
  na.omit

#' And specify the number of observations explicitly.
data_stan_congress <- list(
  N = nrow(df_stan),
  vote_88 = df_stan$DemShare_1988,
  vote_86 = df_stan$DemShare_Adj_1986,
  incumbency_88 = df_stan$Incumbent_1988)

# Compute 2000 posterior samples (4 chains * 1000 iterations * .5 warmup
# iterations).
stan_congress <- stan(
  file = "examples/Ch07/congress.stan",
  data = data_stan_congress,
  iter = 1000,
  chains = 4)

#' Review the model syntax.
stan_congress@stanmodel

#' The parameter estimates are similiar to the R models. We can also see the
#' predictive uncertainty (the stats for `sigma`).
stan_congress

#' ## Simulation for inferences and predictions of new data points (Stan)
#'
#' The 2000 posterior draws are our 2000 simulations, so we just need to
#' estimate the means and add normal noise in each simulation, as we did above.
fit88_post <- rstan::extract(stan_congress)

# Use the same predictor matrix
tail(x_tilde)

# Betas: One row per simulation, one column per beta.
tail(fit88_post$beta)

#' Estimate the means.
est_means <- x_tilde %*% t(fit88_post$beta)

#' Add normal noise to each column of means.
y_tilde <-  matrix(numeric(), nrow = nrow(est_means), ncol = ncol(est_means))
for (simulation in seq_len(ncol(y_tilde))) {
  means_i <- est_means[, simulation]
  sigma_i <- fit88_post$sigma[simulation]
  y_tilde[, simulation] <- rnorm(length(means_i), means_i, sigma_i)
}

#' Compute the number of Democrat wins in each simulation.
n_democrats_tilde <- colSums(.5 < y_tilde)
summary(n_democrats_tilde)
sd(n_democrats_tilde)

p5 <- ggplot() +
  aes(x = n_democrats_tilde) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab("Democrats Elected (Out of 416 races)") +
  ylab("N Simulations (Out of 2000 simulations") +
  theme_bw()
p5


#' ***
sesh_info <- devtools::session_info()

sesh_info$platform

sesh_info$packages
