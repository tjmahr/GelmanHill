source("examples/Ch12/12-Shared.R")

## Complete pooling regression - no random effects
data_list1 <- list(N = length(y), y = y, x = x)

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
lm_no_pool <- lm(formula = y ~ x + factor(county) - 1)
unpooled_intercepts <- coef(lm_no_pool)[-1]
unpooled_slope <- coef(lm_no_pool)[1]
unpooled_se <- summary(lm_no_pool)$coefficients[, "Std. Error"][-1]



### Partial pooling model

data_list2 <- list(N = length(y), y = y, x = x, county = county)
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

# lookup list from county name to county index
display8 <- c(
  `LAC QUI PARLE` = 36,
  AITKIN = 1,
  KOOCHICHING = 35,
  DOUGLAS = 21,
  CLAY = 14,
  STEARNS = 71,
  RAMSEY = 61,
  `ST LOUIS` = 70)

# lookup list from county index to county name
display8_names <- structure(names(display8), names = display8)

# Create a data-frame for plotting the raw data
radon_df <- data.frame(y, x, county)
radon8_df <- subset(radon_df, county %in% display8)
radon8_df$county_name <- display8_names[as.character(radon8_df$county)]

# Order the names based on the original lookup list above
radon8_df$county_name <- factor(radon8_df$county_name, levels = names(display8))

# Create data-frames to hold the estimates from each model
radon8_df_models <- radon8_df[c("county_name", "county")]

radon8_df_models_pooled <- radon8_df_models
radon8_df_models_pooled$int <- pooled[1]
radon8_df_models_pooled$slope <- pooled[2]
radon8_df_models_pooled$model <- "pooled"

radon8_df_models_unpooled <- radon8_df_models
radon8_df_models_unpooled$int <- unpooled_intercepts[radon8_df$county]
radon8_df_models_unpooled$slope <- unpooled_slope
radon8_df_models_unpooled$model <- "unpooled"

radon8_df_models_partial <- radon8_df_models
radon8_df_models_partial$int <- partial_pooled_a_mean[radon8_df$county]
radon8_df_models_partial$slope <- partial_pooled_slope
radon8_df_models_partial$model <- "partial"

# Combine the model data-frames so they can all be plotted by one geom_abline
# call, mapping the model column to the linetype aesthetic to differentiate the
# models.
df_models <- rbind(
  radon8_df_models_pooled,
  radon8_df_models_unpooled,
  radon8_df_models_partial)

y_range <- range(y[county %in% display8])

p1 <- ggplot(radon8_df) +
  aes(factor(x), y) +
  geom_point(position = position_jitter(width = .1, height = 0)) +
  # Switch over to the model fit data-frame
  geom_abline(aes(intercept = int, slope = slope, linetype = model),
              data = df_models) +
  facet_wrap("county_name", ncol = 4) +
  theme_bw()
p1

## No-pooling ests vs. sample size (plot on the left on figure 12.3)
frame1 <- data.frame(
  x1 = sample_sizes_jittered,
  y1 = unpooled_intercepts,
  sd1 = unpooled_se)

p2 <- ggplot(frame1) +
  aes(x = x1, y = y1, ymin = y1 - sd1, ymax = y1 + sd1) +
  geom_point() +
  scale_y_continuous("estimated intercept alpha (no pooling)") +
  scale_x_log10("Sample Size in County j") +
  theme_bw() +
  geom_pointrange()
print(p2)



library("lme4")
# "For example, in the radon model, the hyperparameters are estimated as µ-hat_α
# = 1.46, β-hat = −0.69, σ-hat_y = 0.76, and σ-hat_α = 0.33." [258]
arm::display(lmer(y ~ x + (1 | county)))
#> lmer(formula = y ~ x + (1 | county))
#>             coef.est coef.se
#> (Intercept)  1.46     0.05
#> x           -0.69     0.07
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
