source("examples/Ch12/12-Shared.R")

# Varying-intercept model (no predictors)
data_list1 <- list(
  N = length(mn$log_radon),
  y = mn$log_radon,
  county = mn$county_ind)
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

frame1 <- county_summary %>%
  select(county, x1 = n_jitter, y1 = mean, sd1 = avg_se) %>%
  mutate(model = "1. No Pooling")

lqpc <- frame1 %>%
  filter(county == "LAC QUI PARLE")

## Figure 12.1 (a)
p1 <- ggplot(frame1) +
  aes(x = x1, y = y1) +
  # circle county 36
  geom_point(data = lqpc, shape = 1, size = 30) +
  # mean +/- one SD
  geom_pointrange(aes(ymin = y1 - sd1, ymax = y1 + sd1)) +
  scale_x_continuous("Sample Size in County j", trans = scales::log_trans(),
                     breaks = c(1, 3, 10, 30, 100)) +
  scale_y_continuous("Avg. Log Radon in County j") +
  theme_bw() +
  labs(title = "No Pooling")
print(p1)

## Figure 12.1 (b)

# Overwrite estimates in no pooling frame with newer ones
frame2 <- frame1 %>%
  mutate(y1 = mean_a, sd1 = sd_a, model = "2. Multilevel Model")

lqpc_2 <- frame2 %>%
  filter(county == "LAC QUI PARLE")

p2 <- ggplot(frame2) +
  aes(x = x1, y = y1) +
  # circle county 36
  geom_point(data = lqpc_2, shape = 1, size = 30) +
  # mean +/- one SD
  geom_pointrange(aes(ymin = y1 - sd1, ymax = y1 + sd1)) +
  scale_x_continuous("Sample Size in County j", trans = scales::log_trans(),
                     breaks = c(1, 3, 10, 30, 100)) +
  scale_y_continuous("Avg. Log Radon in County j") +
  theme_bw() +
  labs(title = "Multilevel Model")
print(p2)


## Faceted version

# Combine the data-frames from each plot
both_mods <- rbind(frame1, frame2)

lqpc_3 <- both_mods %>%
  filter(county == "LAC QUI PARLE")

# Horizontal line. (Stan code rescaled mu `a ~ normal (10 * mu_a, sigma_a);`)
est_mu_a <- summary(radon_intercept_sf1, "mu_a")[["summary"]][, "mean"] * 10

p3 <- ggplot(both_mods) +
  aes(x = x1, y = y1) +
  # circle county 36
  geom_point(data = lqpc_3, shape = 1, size = 20) +
  # mean +/- one SD
  geom_pointrange(aes(ymin = y1 - sd1, ymax = y1 + sd1)) +
  geom_hline(yintercept = est_mu_a) +
  facet_grid(. ~ model) +
  scale_x_continuous("Sample Size in County j", trans = scales::log_trans(),
                     breaks = c(1, 3, 10, 30, 100)) +
  scale_y_continuous("Avg. Log Radon in County j") +
  theme_bw() +
  labs(title = "")
p3
