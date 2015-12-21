library("ggplot2")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# "We begin by loading in the data: radon measurements and floors of measurement
# for 919 homes sampled from the 85 counties of Minnesota." [348]
srrs2 <- read.csv("examples/Ch12/srrs2.dat")
head(srrs2)

mn <- srrs2$state == "MN"
radon <- srrs2$activity[mn]

# "Measurements were taken in the lowest living area of each house, with
# basement coded as 0 and first floor coded as 1." [254]
floor <- srrs2$floor[mn]

# "Because it makes sense to assume multiplicative effects, we want to work with
# the logarithms of radon levels; however, some of the radon measurements have
# been recorded as 0.0 picoCuries per liter. We make a simple correction by
# rounding these up to 0.1 before taking logs." [348]
log_radon <- log(ifelse(radon == 0, .1, radon))

# Code the counties from 1 to 85:
# factors for whole dataset => factors for MN subset => factor indices
county_names <- factor(srrs2$county[mn])
county <- as.numeric(county_names)

J <- length(unique(county))
y <- log_radon
x <- floor

sample_sizes <- as.vector(table(county))
sample_sizes_jittered <- sample_sizes * exp(runif(J, -.1, .1))
cty_means <- tapply(y, county, mean)
cty_vars <- tapply(y, county, var)

cty_vars_clean <- cty_vars[!is.na(cty_vars)]
cty_avg_sd <- mean(sqrt(cty_vars_clean))
cty_sds <- cty_avg_sd / sqrt(sample_sizes)
cty_sds_sep <- sqrt(tapply(y, county, var) / sample_sizes)
