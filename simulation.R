source("dgp.R")
source("models.R")

#Simulation size and power
plan <- expand.grid(
  #different beta 3
  spelling = -5:5,
  phonics = -5:5
)
simulate <- function(plan, B = 1000, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  plan$phat.w <- plan$phat.f <- 0
  for (i in 1:nrow(plan)) {
    rejections.w <- matrix(0, nrow = B, ncol = 1)
    rejections.f <- matrix(0, nrow = B, ncol = 1)
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
      rejections.w[b, 1] <- wald.test(fe)
      rejections.f[b, 1] <- f.test(fe)
    }
    plan$phat.w[i] = mean(rejections.w[, 1])
    plan$phat.f[i] = mean(rejections.f[, 1])
    print(round(100 * i / nrow(plan)) / 100)
  }
  plan$MCSE.w <- sqrt(plan$phat.w * (1 - plan$phat.w) / B)
  plan$MCSE.f <- sqrt(plan$phat.f * (1 - plan$phat.f) / B)
  plan
}

rejection.rates <- simulate(
  plan,
  B = 2000
)
saveRDS(rejection.rates, "rejection-rates-sim.RDS")
