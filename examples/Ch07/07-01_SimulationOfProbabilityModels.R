library("rstan")
library("ggplot2")

## A simple example of discrete predictive simulations

# 400 babies are born in a hospital in a year. How many will be girls?
# We know that P(Girl) = .488 and P(Boy) = .512.

# Simulate one year
p_girl <- .488
n_births <- 400
n_girls <- rbinom(1, n_births, p_girl)
n_girls

# Simulate 1000 years
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for (s in seq_len(n_sims)) {
  n_girls[s] <- rbinom(1, n_births, p_girl)
}

hist(n_girls)

# Or tell rbinom to do all 1000 at once
n_girls <- rbinom(1000, n_births, p_girl)

p <- ggplot(data.frame(x1 = n_girls)) +
  aes(x = x1) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5) +
  xlab("Number of Girls Born in Simulated Year") +
  theme_bw()
p


## Accounting for twins

p_frat_twin <- 1 / 125
p_identical_twin <- 1 / 300
p_singleton <- 1 - p_frat_twin - p_identical_twin

p_girl_singleton <- .488
p_girl_twin <- .495

simulate_year <- function(n_births) {
  birth_type <- sample(
    x = c("fraternal twin", "identical twin", "single birth"),
    size = n_births,
    replace = TRUE,
    prob = c(p_frat_twin, p_identical_twin, p_singleton))

  girls <- rep(NA, n_births)
  for (i in seq_len(n_births)) {
    if (birth_type[i] == "single birth") {
      girls[i] <- rbinom(1, 1, p_girl)
    }
    else if (birth_type[i] == "identical twin") {
      # two-for-one draw
      girls[i] <- 2 * rbinom(1, 1, p_girl_twin)
    }
    else if (birth_type[i] == "fraternal twin") {
      # two draws
      girls[i] <- rbinom(1, 2, p_girl_twin)
    }
  }

  sum(girls)
}

simulate_year(400)
n_girls <- replicate(1000, simulate_year(400))

p %+% (data.frame(x1 = n_girls))



## A simple example of continuos predictive simulations

# You mean 10 randum adults. What's their average height?

n_adults <- 10
p_woman <- .52
men_mean <- 69.1
men_sd <- 2.9
women_mean <- 64.5
women_sd <- 2.7

simulate_heights <- function(n, p_w = p_woman, avg_m = men_mean, sd_m = men_sd,
                             avg_w = women_mean, sd_w = women_sd) {
  # First draw sexes
  woman <- rbinom(n, 1, p_w)
  # For each sex draw a height using sex's mean/sd
  heights <- ifelse(woman == 0, rnorm(n, avg_m, sd_m),
                    rnorm(n, avg_w, sd_w))
  heights
}

simulate_heights(10)

# simulation for average height
heights <- replicate(1000, mean(simulate_heights(10)))

p1 <- ggplot(data.frame(x1 = heights)) +
  aes(x = x1) +
  geom_histogram(colour = "black", fill = "white", binwidth = .5) +
  xlab("Average height of 10 adults") +
  theme_bw()
p1

# simulation for maximum height
heights <- replicate(1000, max(simulate_heights(10)))

p2 <- (p1 %+% data.frame(x1 = heights)) +
  xlab("Maximum height of 10 adults")
p2
