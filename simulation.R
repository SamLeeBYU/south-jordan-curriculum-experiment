source("dgp.R")
source("models.R")

#Simulation size and power
plan <- expand.grid(
  #different beta 3
  spelling = c(-10, -5, 0, 5, 10),
  phonics = c(-10, -5, 0, 5, 10)
)
simulate <- function(plan, B = 10000, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  plan$phatFE <- plan$phatRE <- 0
  for (i in 1:nrow(plan)) {
    rejections <- matrix(0, nrow = B, ncol = 2)
    plan.i <- plan[i, ]
    for (b in 1:B) {
      ellas.class <- generate_classroom_data(
        rho = 0.9,
        sigma_s = 1,
        sigma_p = 1,
        ability_sd = 1,
        lambda_week = c(`1` = 0, `2` = 0),
        kappa_class = c(blue = 3, red = 0),

        beta1 = c(spelling = 4, phonics = 4), # effect of post indicator
        beta2 = c(spelling = 0, phonics = 0), # main effect of curated
        beta3 = c(spelling = plan.i$spelling, phonics = plan.i$phonics), # post x curated gain

        seed = NULL
      )

      fe <- fit.lm(ellas.class)
      re <- fit.re(ellas.class)
      rejections[b, 1:2] <- c(wald.test(fe), wald.test(re))
    }
    plan$phatFE[i] = mean(rejections[, 1])
    plan$phatRE[i] = mean(rejections[, 2])
    print(round(100 * i / nrow(plan)) / 100)
  }
  plan$MCSEFE <- sqrt(plan$phatFE * (1 - plan$phatFE) / B)
  plan$MCSERE <- sqrt(plan$phatRE * (1 - plan$phatRE) / B)
  plan
}

rejection.rates <- simulate(plan, B = 1000)
