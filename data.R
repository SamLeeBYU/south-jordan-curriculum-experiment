source("dgp.R")

ellas.class <- generate_classroom_data(
  rho = 0.9,
  sigma_s = 1,
  sigma_p = 1,
  ability_sd = 1,
  lambda_week = c(`1` = 0, `2` = 0),
  kappa_class = c(blue = 3, red = 0),

  beta1 = c(spelling = 4, phonics = 4), # effect of post indicator
  beta2 = c(spelling = 0, phonics = 0), # main effect of curated
  beta3 = c(spelling = 3, phonics = 3), # post x curated gain

  seed = 666
)
