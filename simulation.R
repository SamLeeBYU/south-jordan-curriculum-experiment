source("dgp.R")
source("models.R")

#Simulation size and power
plan <- expand.grid(
  #different beta 3
  spelling = -5:5,
  phonics = -5:5
)
simulate <- function(plan, B = 100, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  plan$phatFE <- 0
  for (i in 1:nrow(plan)) {
    rejections <- matrix(0, nrow = B, ncol = 1)
    plan.i <- plan[i, ]
    for (b in 1:B) {
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
        beta3 = c(spelling = plan.i$spelling, phonics = plan.i$phonics), # post x curated gain

        seed = NULL
      )

      fe <- fit.lm(ellas.class)
      rejections[b, 1] <- wald.test(fe)
    }
    plan$phatFE[i] = mean(rejections[, 1])
    print(round(100 * i / nrow(plan)) / 100)
  }
  plan$MCSEFE <- sqrt(plan$phatFE * (1 - plan$phatFE) / B)
  plan
}

rejection.rates <- simulate(plan, B = 10000)
saveRDS(rejection.rates, "rejection-rates-sim.RDS")
