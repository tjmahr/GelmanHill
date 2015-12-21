source("examples/Ch12/12-Shared.R")

## Complete pooling regression - no random effects
data_list1 <- list(
  N = length(mn$log_radon),
  y = mn$log_radon,
  x = mn$floor)
str(data_list1)

radon_complete_pool_sf1 <- stan(
  file = 'examples/Ch12/radon_complete_pool.stan',
  data = data_list1,
  iter = 1000,
  chains = 4)

radon_complete_pool_sf1@stanmodel
print(radon_complete_pool_sf1)

post_pooled <- extract(radon_complete_pool_sf1)
pooled <- colMeans(post_pooled$beta)


## No pooling regression

# The Stan model that was here showed shrinkage. (The intercept for Lac Qui
# Parle was suppressed, and didn't recreate the plot in fig. 12.02.) So let's
# fit the model with the R code used in the book instead.
lm_no_pool <- lm(log_radon ~ 0 + floor + county, mn)
unpooled_intercepts <- coef(lm_no_pool)[-1]
unpooled_slope <- coef(lm_no_pool)[1]
unpooled_se <- summary(lm_no_pool)$coefficients[, "Std. Error"][-1]


## Partial pooling model
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


## Comparing-complete pooling & no-pooling (Figure 12.4, 12.3)

# Caption: "Figure 12.2 Complete-pooling (dashed lines, y = α + βx) and
# no-pooling (solid lines, y = αj + βx) regressions fit to radon data from the
# 85 counties in Minnesota, and displayed for eight of the counties. The
# estimated slopes β differ slightly for the two models, but here our focus is
# on the intercepts." [255]

# "...a selection of eight counties, chosen to capture a range of the sample
# sizes in the survey" [254]

# Caption: "Figure 12.4 Multilevel (partial pooling) regression lines y = αj +
# βx fit to radon data from Minnesota, displayed for eight counties." [257]

display8_names <- c("LAC QUI PARLE", "AITKIN", "KOOCHICHING", "DOUGLAS", "CLAY",
                    "STEARNS", "RAMSEY", "ST LOUIS")

counties <- mn %>%
  select(county, county_ind) %>%
  distinct %>%
  arrange(county_ind)

# Create data-frames summarising each model
models <- list()

models$pooled <- counties
models$pooled$int <- pooled[1]
models$pooled$slope <- pooled[2]
models$pooled$model <- "pooled"

models$unpooled <- counties
models$unpooled$int <- unpooled_intercepts
models$unpooled$slope <- unpooled_slope
models$unpooled$model <- "unpooled"

models$partial <- counties
models$partial$int <- partial_pooled_a_mean
models$partial$slope <- partial_pooled_slope
models$partial$model <- "partial"

# Combine model summaries, keeping just rows for the 8 counties
radon8_df_models <- rbind_all(models) %>%
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

## No-pooling ests vs. sample size (plot on the left on figure 12.3)
frame1 <- county_summary %>%
  select(county, x1 = n_jitter) %>%
  mutate(
    y1 = unpooled_intercepts,
    sd1 = unpooled_se)

p2 <- ggplot(frame1) +
  aes(x = x1, y = y1, ymin = y1 - sd1, ymax = y1 + sd1) +
  geom_point() +
  geom_pointrange() +
  scale_x_continuous("Sample Size in County j", trans = scales::log_trans(),
                     breaks = c(1, 3, 10, 30, 100)) +
  scale_y_continuous("estimated intercept alpha (no pooling)") +
  theme_bw()
print(p2)

# "For example, in the radon model, the hyperparameters are estimated as µ-hat_α
# = 1.46, β-hat = −0.69, σ-hat_y = 0.76, and σ-hat_α = 0.33." [258]
arm::display(lmer(log_radon ~ floor + (1 | county), mn))
#> lmer(formula = log_radon ~ floor + (1 | county), data = mn)
#>             coef.est coef.se
#> (Intercept)  1.46     0.05
#> floor       -0.69     0.07
#>
#> Error terms:
#>  Groups   Name        Std.Dev.
#>  county   (Intercept) 0.33
#>  Residual             0.76
#> ---
#> number of obs: 919, groups: county, 85
#> AIC = 2179.3, DIC = 2156
#> deviance = 2163.7

# Intraclass correlation = σ_α^2 / (σ_α^2 + σ_y^2)
(0.33 ^ 2) / ((0.33 ^ 2) + (0.76 ^ 2))
