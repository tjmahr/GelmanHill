source("examples/Ch12/12-Shared.R")

counties <- mn %>%
  select(county, county_ind, uranium) %>%
  distinct %>%
  arrange(county_ind)

## Varying-intercept model w/ group-level predictors

data_list3 <- list(
  N = length(mn$log_radon),
  y = mn$log_radon,
  x = mn$floor,
  county = mn$county_ind,
  u = mn$uranium)
str(data_list3)

radon_group_sf1 <- stan(
  file = 'examples/Ch12/radon_group.stan',
  data = data_list3,
  iter = 1000,
  chains = 4)

# Warning messages:
# 1: There were 182 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help.
# 2: Examine the pairs() plot to diagnose sampling problems

# View model that was fit
radon_group_sf1@stanmodel

print(radon_group_sf1, pars = c("b","beta", "sigma", "lp__"))
post1 <- extract(radon_group_sf1)

post1_ranef <- colMeans(post1$b)
post1_beta <- colMeans(post1$beta)

str(post1)

model1 <- counties %>%
  mutate(
    int = post1_ranef + post1_beta[2] * uranium,
    slope = post1_beta[1],
    model = "partial")


## Plots on Figure 12.5

# Partial pooling model
data_list2 <- list(
  N = length(mn$log_radon),
  y = mn$log_radon,
  x = mn$floor,
  county = mn$county_ind)
str(data_list2)

radon_partial_pool_sf1 <- stan(
  file = 'examples/Ch12/radon_no_pool.stan',
  data = data_list2,
  iter = 1000,
  chains = 4)

radon_partial_pool_sf1@stanmodel

# Don't print individual estimates (y-hat)
print(radon_partial_pool_sf1, c("a", "beta", "sigma_a", "sigma_y", "mu_a"))

partial_pooled <- extract(radon_partial_pool_sf1)

partial_pooled_slope <- mean(partial_pooled$beta)
partial_pooled_a_mean <- apply(partial_pooled$a, 2, mean)
partial_pooled_a_sd <- apply(partial_pooled$a, 2, sd)

model2 <- counties %>%
  mutate(
    int = partial_pooled_a_mean,
    slope = partial_pooled_slope,
    model = "uranium")

display8_names <- c("LAC QUI PARLE", "AITKIN", "KOOCHICHING", "DOUGLAS", "CLAY",
                    "STEARNS", "RAMSEY", "ST LOUIS")

# Combine model summaries, keeping just rows for the 8 counties
radon8_df_models <- rbind(model2, model1) %>%
  filter(county %in% display8_names) %>%
  mutate(county = factor(county, levels = display8_names))

# Create a data-frame for plotting the raw data for the 8 counties
radon8_df <- mn %>%
  select(county, county_ind, log_radon, floor) %>%
  filter(county %in% display8_names) %>%
  mutate(county = factor(county, levels = display8_names))


p1 <- ggplot(radon8_df) +
  aes(floor, log_radon) +
  geom_point(position = position_jitter(width = .1, height = 0)) +
  # Switch over to the model fit data-frame
  geom_abline(aes(intercept = int, slope = slope, linetype = model),
              data = radon8_df_models) +
  facet_wrap("county", ncol = 4) +
  scale_x_continuous("floor", breaks = c(0, 1)) +
  theme_bw()
p1


# Plot of ests & se's vs. county uranium (Figure 12.6)
m_vim_grp <- lmer(log_radon ~ floor + uranium + (1 | county), mn)
display(m_vim_grp)

counties$int_coef <- coef(m_vim_grp)$county[["(Intercept)"]]
counties$uranium_beta <- fixef(m_vim_grp)["uranium"]
counties$int_se <- se.coef(m_vim_grp)[["county"]][, 1]

counties <- counties %>%
  mutate(
    int_est = int_coef + uranium * uranium_beta,
    y1 = int_est - int_se,
    y2 = int_est + int_se)

line_int <- unname(fixef(m_vim_grp)["(Intercept)"])
line_slope <- unname(fixef(m_vim_grp)["uranium"])
ggplot(counties) +
  aes(x = uranium, y = int_est) +
  geom_point() +
  geom_pointrange(aes(ymin = y1, ymax = y2)) +
  geom_abline(aes(intercept = line_int, slope = line_slope)) +
  theme_bw() +
  xlab("county-level uranium measure") +
  ylab("est. regression intercept")
