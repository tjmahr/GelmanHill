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

# Varying-intercept model (no predictors)
data_list1 <- list(N = length(y), y = y, county = county)
radon_intercept_sf1 <- stan(
  file = 'examples/Ch12/radon_intercept.stan',
  data = data_list1,
  iter = 1000,
  chains = 4)

# View model that was fit
radon_intercept_sf1@stanmodel

# Don't print individual estimates (y-hat)
print(radon_intercept_sf1, c("a", "mu_a", "sigma_a", "sigma_y"))

# Extract samples from model
post <- extract(radon_intercept_sf1)

# columnwise mean_and sd of a's. (columns are counties, rows are samples)
mean_a <- apply(post$a, 2, mean)
sd_a <- apply(post$a, 2, sd)




## Figure 12.1

# Caption: "Figure 12.1 Estimates ± standard errors for the average log radon
# levels in Minnesota counties plotted versus the (jittered) number of
# observations in the county: (a) no-pooling analysis, (b) multilevel (partial
# pooling) analysis, in both cases with no house-level or county-level
# predictors. The counties with fewer measurements have more variable esti-
# mates and larger higher standard errors. The horizontal line in each plot
# represents an estimate of the average radon level across all counties. The
# left plot illustrates a problem with the no-pooling analysis: it
# systematically causes us to think that certain counties are more extreme, just
# because they have smaller sample sizes." [253]

# "The no-pooling analysis overfits the data within each county. To see this,
# consider Lac Qui Parle County (circled in the plot), which has the highest
# average radon level of all 85 counties in Minnesota. This average, however, is
# estimated using only two data points." [253]

frame1 = data.frame(
  x1 = sample_sizes_jittered,
  y1 = cty_means,
  sd1 = cty_sds,
  # 36 is Lac Qui Parle County
  xi_36 = sample_sizes_jittered[36],
  yi_36 = cty_means[36],
  model = "No Pooling"
)

## Figure 12.1 (a)
p1 <- ggplot(frame1) +
  aes(x = x1, y = y1) +
  # circle county 36
  geom_point(aes(x = xi_36, y = yi_36), shape = 1, size = 30) +
  # mean +/- one SD
  geom_pointrange(aes(ymin = y1 - sd1, ymax = y1 + sd1)) +
  scale_x_log10("Sample Size in County j") +
  scale_y_continuous("Avg. Log Radon in County j") +
  theme_bw() +
  labs(title = "No Pooling")
print(p1)

## Figure 12.1 (b)
frame2 <- data.frame(
  x1 = sample_sizes_jittered,
  y1 = mean_a,
  sd1 = sd_a,
  xi_36 = sample_sizes_jittered[36],
  yi_36 = mean_a[36],
  model = "Multilevel Model")

p2 <- ggplot(frame2) +
  aes(x = x1, y = y1) +
  # circle county 36
  geom_point(aes(x = xi_36, y = yi_36), shape = 1, size = 30) +
  # mean +/- one SD
  geom_pointrange(aes(ymin = y1 - sd1, ymax = y1 + sd1)) +
  scale_x_log10("Sample Size in County j") +
  scale_y_continuous("Avg. Log Radon in County j") +
  theme_bw() +
  labs(title = "Multilevel Model")
print(p2)


## Faceted version

# Combine the data-frames from each plot
both_mods <- rbind(frame1, frame2)
both_mods$model <- factor(both_mods$model, c("No Pooling", "Multilevel Model"))

# Horizontal line. (Stan code rescaled mu `a ~ normal (10 * mu_a, sigma_a);`)
est_mu_a <- summary(radon_intercept_sf1, "mu_a")[["summary"]][, "mean"] * 10

# Replace data plot1 with the combined data, then facet by model name
p3 <- (p1 %+% both_mods) +
  facet_grid(. ~ model) +
  labs(title = "") +
  geom_hline(yintercept = est_mu_a)
p3
