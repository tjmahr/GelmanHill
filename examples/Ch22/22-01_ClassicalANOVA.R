library(rstan)
library(ggplot2)

## Read the pilots data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots

pilots <- read.table("./examples/Ch22/pilots.dat", header = TRUE)
attach(pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- group==group.names[j] & scenario==scenario.names[k]
    successes <- c (successes, sum(recovered[ok]==1,na.rm=T))
    failures <- c (failures, sum(recovered[ok]==0,na.rm=T))
    group.id <- c (group.id, j)
    scenario.id <- c (scenario.id, k)
  }
}

y <- successes/(successes+failures)
y.mat <- matrix (y, n.scenario, n.group)
sort.group <- order(apply(y.mat,2,mean))
sort.scenario <- order(apply(y.mat,1,mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario,sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Classical anova of pilots data
data <- data.frame(y, treatment = factor(treatment), airport = factor(airport))
summary(aov(y ~ treatment + airport, data))
plot(aov(y ~ treatment + airport, data))

library(rstanarm)
post1 <- stan_aov(
  y ~ treatment + airport,
  data,
  prior = R2(location = 0.5))
post1

# Bayesian formulation
post2 <- stan_glmer(
  y ~ 1 + (1 | treatment) + (1 | airport),
  family = gaussian,
  data)
post2

# Compare to Figure 22.5
bayesplot::mcmc_intervals(as.matrix(post2), regex_pars = "sigma|Sigma")
