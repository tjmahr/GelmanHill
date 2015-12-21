library("arm")
library("dplyr")
library("ggplot2")
library("stringr")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# "We begin by loading in the data: radon measurements and floors of measurement
# for 919 homes sampled from the 85 counties of Minnesota." [348]
mn <- read.csv("examples/Ch12/srrs2.dat", stringsAsFactors = FALSE) %>%
  as.tbl %>%
  filter(state == "MN") %>%
  mutate(county = str_trim(county)) %>%
  select(county, radon = activity, floor)
mn

# "Measurements were taken in the lowest living area of each house, with
# basement coded as 0 and first floor coded as 1." [254]

# "Because it makes sense to assume multiplicative effects, we want to work with
# the logarithms of radon levels; however, some of the radon measurements have
# been recorded as 0.0 picoCuries per liter. We make a simple correction by
# rounding these up to 0.1 before taking logs." [348]
mn <- mn %>%
  mutate(log_radon = log(ifelse(radon == 0, .1, radon)),
         # Code the counties from 1 to 85
         county_ind = as.numeric(factor(county)))


# srrs2 <- read.csv("examples/Ch12/srrs2.dat")
# mn <- srrs2$state == "MN"
# radon <- srrs2$activity[mn]
# floor <- srrs2$floor[mn]
#
# county_names <- factor(srrs2$county[mn])
# county <- as.numeric(county_names)

# J <- n_distinct(mn$county)
# y <- mn$log_radon
# x <- mn$floor

# # Original definitions
# sample.size <- as.vector(table(county))
# sample.size.jittered <- sample.size * exp (runif (J,-.1, .1))
# cty.mns <- tapply(y, county, mean)
# cty.vars <- tapply(y, county, var)
# cty.sds <- mean(sqrt(cty.vars[!is.na(cty.vars)])) / sqrt(sample.size)
# cty.sds.sep = sqrt(tapply(y, county, var) / sample.size)

# I'm still not sure whether/how the book motivates the computation of "cty.sds"
# (called "avg_se" in the dataframe below) as an estimate of the standard error
# of the mean in each county

# new names of variables
# .$mean was cty.mns
# .$var was cty.vars
# .$avg_sd was mean(sqrt(cty.vars[!is.na(cty.vars)]))
# .$avg_se was mean(sqrt(cty.vars[!is.na(cty.vars)])) / sqrt(sample.size)
# .$se was cty.sds.sep

exp_jitter <- function(xs, amount) {
  xs * exp(runif(length(xs), -amount, amount))
}

county_summary <- mn %>%
  group_by(county) %>%
  # county-level stats
  summarise(
    n = n(),
    mean = mean(log_radon),
    var = var(log_radon),
    sds = sd(log_radon)) %>%
  # derived values
  mutate(
    se = sds / sqrt(n),
    avg_sd = mean(sds, na.rm = TRUE),
    avg_se = avg_sd / sqrt(n),
    n_jitter = exp_jitter(n, .1)) %>%
  arrange(county)

