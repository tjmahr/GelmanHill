Data
====

1. radon.data.R
  - J     : number of counties
  - N     : number of observations
  - county: county number 
  - radon : radon measurement
  - u     : county-level uranium measure
  - x     : house-level first-floor indicator
  - y     : log of the home radon level

Models
======

1. One predictor  
  radon_complete_pool.stan: lm(y ~ x)

2. Multilevel model with varying intercept  
  radon_group.stan: lmer(y ~ x + u + (1 | county))  
  radon_intercept.stan: lmer(y ~ 1 + (1 | county))  
  radon_no_pool.stan: lmer(y ~ x + (1 | county))  

3. Above models with Matt trick  
  radon_group_chr.stan: lmer(y ~ x + u + (1 | county))  
  radon_intercept_chr.stan: lmer(y ~ 1 + (1 | county))  
  radon_no_pool_chr.stan: lmer(y ~ x + (1 | county))  
