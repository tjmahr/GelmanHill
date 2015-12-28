#' # 07-03 Simulation for Nonlinear Predictions

# + setup, include = FALSE
library("knitr")
opts_chunk$set(collapse = TRUE, comment = "#>", fig.path = "figure/07-03/")
opts_knit$set(root.dir = "../../")

# + warning = FALSE, message = FALSE
library("ggplot2")
library("dplyr")
library("tidyr")
library("readr")
library("stringr")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Code to load a single asc file
read_asc_year <- function(path) {
  # Get year from filename
  year <- path %>% basename %>% str_extract("\\d{4}") %>% as.numeric

  df <- path %>%
    # It looks like -9 was used to code missing values.
    read_table(col_names = FALSE, na = "-9") %>%
    # Make sure all values are numbers so division works
    mutate_each(funs(as.numeric)) %>%
    rename(Incumbent = X3, DemVote = X4, RepVote = X5) %>%
    mutate(year = year,
           DemShare = DemVote / (DemVote + RepVote),
           Contested = 0.1 < DemShare & DemShare < 0.9,
           # Impute uncontested values: Create an adjusted share to show what
           # might have happened if the race had been contested.
           DemShare_Adj = ifelse(DemShare < 0.1, .25, DemShare),
           DemShare_Adj = ifelse(0.9 < DemShare_Adj, .75, DemShare_Adj)) %>%
    select(year, everything())
  df
}

# path <- paths[46]
paths <- list.files(path = "examples/Ch07/cong3/", full.names = TRUE)

# Old code loaded each asc file into a list of matrices. We're combining all the
# files into a single data-frame.
congress <- paths %>%
  lapply(read_asc_year) %>%
  bind_rows %>%
  filter(year %in% c(1986, 1988, 1990))

# congress <- vector("list", 49)
# for (i in 1:49) {
#   file <- "examples/Ch07/cong3/1896.asc"
#
#   readr::read_table(file, col_names = FALSE)
#
#   year <- 1896 + 2 * (i - 1)
#   file <- paste0("examples/Ch07/cong3/", year, ".asc", sep = "")
#   data.year <- matrix(scan(file), byrow = TRUE, ncol = 5)
#   data.year <- cbind(rep(year, nrow(data.year)), data.year)
#   congress[[i]] <- data.year
# }

# Note: download all '.asc' files into your R working directory in a file named
# cong3 for the above command to work

# i86 <- (1986 - 1896) / 2 + 1
# cong86 <- congress[[i86]]
# cong88 <- congress[[i86 + 1]]
# cong90 <- congress[[i86 + 2]]

# v86 <- cong86[, 5] / (cong86[, 5] + cong86[, 6])
# bad86 <- cong86[, 5] == -9 | cong86[, 6] == -9
# v86[bad86] <- NA
# contested86 <- 0.1 < v86 & v86 < 0.9
# inc86 <- cong86[, 4]
#
# v88 <- cong88[, 5] / (cong88[, 5] + cong88[, 6])
# bad88 <- cong88[, 5] == -9 | cong88[, 6] == -9
# v88[bad88] <- NA
# contested88 <- v88 > 0.1 & v88 < 0.9
# inc88 <- cong88[, 4]
#
# v90 <- cong90[, 5] / (cong90[, 5] + cong90[, 6])
# bad90 <- cong90[, 5] == -9 | cong90[, 6] == -9
# v90[bad90] <- NA
# contested90 <- v90 > 0.1 & v90 < 0.9
# inc90 <- cong90[, 4]

# jitt <- function(x, delta) {
#   x + runif(length(x),-delta, delta)
# }

## Plot Figure 7.3
v88_hist <- congress %>%
  filter(year == 1988) %>%
  # Turn the uncontested proportions into landslides, for presentation purposes
  mutate(DemShare_flat = ifelse(DemShare < 0.1, 1e-04, DemShare),
         DemShare_flat = ifelse(0.9 < DemShare, 0.9999, DemShare))

#
# v88.hist <- ifelse(v88 < 0.1, 1e-04, ifelse(v88 > 0.9, 0.9999, v88))

#+ warning = FALSE
p1 <- ggplot(v88_hist) +
  aes(x = DemShare_flat) +
  geom_histogram(colour = "black", fill = "white", binwidth = 0.05) +
  scale_x_continuous("Democratic Share of the Two-Party Vote") +
  labs(title = "Congressional Elections in 1988") +
  theme_bw()
p1


## Fitting the model (congress.stan)
## lm (vote.88 ~ vote.86 + incumbency.88)
year_by_year <- congress %>%
  filter(year %in% c(1986, 1988)) %>%
  select(-RepVote, -DemVote) %>%
  gather(Key, Value, -year, -X1, -X2) %>%
  unite(Key_Year, Key, year) %>%
  spread(Key_Year, Value)

df_m <- year_by_year %>%
  filter(Contested_1988 == 1)

# year_by_year
# v86 <- v86$X4
# contested88 <- as.vector(contested88)
# inc88 <- inc88$X3
#
#
#
# v86.adjusted <- ifelse(v86 < 0.1, 0.25, ifelse(v86 > 0.9, 0.75, v86))
# vote.86 <- v86.adjusted[contested88]
#
# incumbency.88 <- inc88[contested88]
# vote.88 <- v88$DemShare[contested88]
# ok <- !is.na(vote.86 + incumbency.88 + vote.88)

summary(lm(DemShare_Adj_1988 ~ DemShare_Adj_1986 + Incumbent_1988, df_m))

# summary(lm(vote.88 ~ vote.86 + incumbency.88))
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.201998   0.018200   11.10   <2e-16 ***
## vote.86       0.583276   0.035045   16.64   <2e-16 ***
## incumbency.88 0.077052   0.007036   10.95   <2e-16 ***
## ---

df_stan <- df_m %>%
  select(DemShare_Adj_1986, Incumbent_1988, DemShare_1988) %>%
  na.omit
# ok <- !is.na(vote.86 + incumbency.88 + vote.88)

data_list1 <- list(
  N = nrow(df_stan),
  vote_88 = df_stan$DemShare_1988,
  vote_86 = df_stan$DemShare_Adj_1986,
  incumbency_88 = df_stan$Incumbent_1988)

stan_congress <- stan(
  file = "examples/Ch07/congress.stan",
  data = data_list1,
  iter = 1000,
  chains = 4)
print(stan_congress)

fit88_post <- rstan::extract(stan_congress)

## Figure 7.4

# 7.4 (a)
# j.v86 <- ifelse(contested86, v86, jitter(v86, amount = 0.02))
# j.v88 <- ifelse(contested88, v88, jitter(v88, amount = 0.02))
#
# frame1 <- data.frame(x1 = j.v86[inc88 == 0], y1 = j.v88[inc88 == 0])
# frame2 <- data.frame(x2 = j.v86[inc88 == 1], y2 = j.v88[inc88 == 1])
# frame3 <- data.frame(x3 = j.v86[inc88 == -1], y3 = j.v88[inc88 == -1])
#
# dev.new()

names(year_by_year) <- names(year_by_year) %>%
  str_replace("Contested", "Con") %>%
  str_replace("DemShare_", "Dem_") %>%
  str_replace("Incumbent", "Inc")

# Jitter uncontested points
df_plot <-
  year_by_year %>%
  mutate(Jit_86 = ifelse(Con_1986, Dem_1986, jitter(Dem_1986, amount = 0.02)),
         Jit_88 = ifelse(Con_1988, Dem_1988, jitter(Dem_1988, amount = 0.02)))

p2 <- ggplot(df_plot) +
  aes(x = Jit_86, Jit_88, shape = factor(Inc_1988)) +
  geom_point() +
  scale_shape_manual(values = c(1, 16, 4)) +
  xlab("Democratic Vote Share in 1986") +
  ylab("Democratic Vote Share in 1988") +
  labs(title = "Raw Data (jittered at 0 and 1)") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

# 7.4 (b)
# v86.adjusted <-
#   ifelse(v86 < 0.1, 0.25, ifelse(v86 > 0.9, 0.75, v86))
# vote.86 <- v86.adjusted[contested88]
# vote.88 <- v88[contested88]
# incumbency.88 <- inc88[contested88]
#
# frame4 <- data.frame(x1 = vote.86[incumbency.88 ==  0], y1 = vote.88[incumbency.88 ==  0])
# frame5 <- data.frame(x2 = vote.86[incumbency.88 ==  1], y2 = vote.88[incumbency.88 ==  1])
# frame6 <- data.frame(x3 = vote.86[incumbency.88 == -1], y3 = vote.88[incumbency.88 == -1])


p3 <- ggplot(df_m) +
  aes(x = DemShare_Adj_1986, DemShare_Adj_1988, shape = factor(Incumbent_1988)) +
  geom_point() +
  scale_shape_manual(values = c(1, 16, 4)) +
  xlab("Democratic Vote Share in 1986") +
  ylab("Democratic Vote Share in 1988") +
  labs(title = "Adjusted data (imputing 0's to .25 and 1's to .75)") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
p3






## Simulation for inferences and predictions of new data points

incumbency.90 <- inc90
vote.88 <- v88
n.tilde <- length(vote.88)
X.tilde <- cbind(rep(1, n.tilde), vote.88, incumbency.90)

n.sims <- 4000
sim.88 <- fit88_post$beta
y.tilde <- array(NA, c(n.sims, n.tilde))
for (s in 1:n.sims) {
  pred <- X.tilde %*% fit88_post$beta[s,]
  ok <- !is.na(pred)
  y.tilde[s, ok] <- rnorm(sum(ok), pred[ok], fit88_post$sigma[s])
}

## Predictive simulation for a nonlinear function of new data

y.tilde.new <- ifelse(y.tilde == "NaN", 0, y.tilde)

dems.tilde <- rowSums(y.tilde.new > 0.5)
# or
dems.tilde <- rep(NA, n.sims)
for (s in 1:n.sims) {
  dems.tilde[s] <- sum(y.tilde.new[s,] > 0.5)
}

## Implementation using functions

Pred.88 <- function(X.pred) {
  pred <- X.tilde %*% t(fit88_post$beta)
  ok <- !is.na(pred)
  n.pred <- length(pred)
  y.pred <- rep(NA, n.pred)
  y.pred[ok] <- rnorm(sum(ok), pred[ok], fit88_post$sigma)
  return(y.pred)
}

y.tilde <- replicate(1000, Pred.88(X.tilde))
dems.tilde <- replicate(1000, Pred.88(X.tilde) > 0.5)
