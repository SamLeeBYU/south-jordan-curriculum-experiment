library(rstanarm)

source("dgp.R")

ellas.class <- generate_classroom_data(
  rho = 0.9,
  sigma_s = 2,
  sigma_p = 2,
  ability_sd = 5,
  
  #This is effectively deviations in mu_0
  test_effect = c(spelling = 2, phonics = -2),
  
  lambda_week = c(`1` = 0, `2` = 0),
  kappa_class = c(blue = 5, red = 0),
  
  beta1 = c(spelling = 0, phonics = 0), # effect of post indicator
  beta2 = c(spelling = 0, phonics = 0), # main effect of curated
  beta3 = c(spelling = 3, phonics = 3), # post x curated gain
  
  seed = 666
)

fit_mv <- stan_mvmer(
  formula = list(
    spelling ~ 0 +
      factor(week) +
      factor(classroom) +
      factor(time) * factor(instruction) +
      (1 | id),

    phonics   ~ 0 +
      factor(week) +
      factor(classroom) +
      factor(time) * factor(instruction) +
      (1 | id)
  ),
  data   = ellas.class,
  family = gaussian(),
  prior  = normal(0, 5, autoscale = TRUE),
  prior_intercept = normal(0, 5, autoscale = TRUE),
  prior_covariance = decov(regularization = 2),
  chains = 4, iter = 100000, cores = 4,
  refresh = T
)

allsamps <- as.matrix(fit_mv)
saveRDS(allsamps, "mcmc-samples.rds")
