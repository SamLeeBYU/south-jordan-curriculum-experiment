library(dplyr)
library(tidyr)

#This DGP was originally written by Gavin Hatch,
# recoded by Sam Lee to match regression model in the paper

library(dplyr)
library(MASS)

generate_classroom_data <- function(
  n_per_class = 18,
  classes = c("blue", "red"),

  # student ability
  ability_mean = 70,
  ability_sd = 6,

  # test-specific shift
  test_effect = c(spelling = 2, phonics = -2),

  # week and class fixed effects
  lambda_week = c(`1` = 0, `2` = 1),
  kappa_class = c(blue = 0, red = 0),

  # regression parameters
  beta1 = c(spelling = 4, phonics = 4), # effect of post indicator
  beta2 = c(spelling = 0, phonics = 0), # main effect of curated
  beta3 = c(spelling = 3, phonics = 3), # post x curated gain

  # bivariate error covariance
  sigma_s = 4, # spelling SD
  sigma_p = 4, # phonics SD
  rho = 0, # covariance term
  clip_lo = 0,
  clip_hi = 100,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # error covariance matrix
  Sigma_u <- matrix(
    c(sigma_s^2, rho, rho, sigma_p^2),
    nrow = 2,
    byrow = TRUE
  )
  L.u <- chol(Sigma_u)

  # cross-over assignment
  instruction_fun <- function(classroom, week) {
    if (week == 1 && classroom == "blue") {
      return("traditional")
    }
    if (week == 1 && classroom == "red") {
      return("curated")
    }
    if (week == 2 && classroom == "blue") {
      return("curated")
    }
    if (week == 2 && classroom == "red") {
      return("traditional")
    }
  }

  # students and design
  students <- data.frame(
    id = 1:(length(classes) * n_per_class),
    classroom = rep(classes, each = n_per_class)
  )

  design <- expand.grid(
    id = students$id,
    week = 1:2,
    time = c("pre", "post"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    left_join(students, by = "id") %>%
    mutate(
      instruction = mapply(instruction_fun, classroom, week),
      week = factor(week),
      time = factor(time, levels = c("pre", "post")),
      instruction = factor(instruction, levels = c("traditional", "curated")),
      classroom = factor(classroom)
    )

  # student abilities
  ability_student_s <- rnorm(
    nrow(students),
    mean = ability_mean + test_effect["spelling"],
    sd = ability_sd
  )
  ability_student_p <- rnorm(
    nrow(students),
    mean = ability_mean + test_effect["phonics"],
    sd = ability_sd
  )
  names(ability_student_s) <- students$id
  names(ability_student_p) <- students$id

  clip01 <- function(x, lo = clip_lo, hi = clip_hi) pmin(pmax(x, lo), hi)

  alpha.mat <- matrix(
    c(ability_student_s[design$id], ability_student_p[design$id]),
    ncol = 2
  )
  lambda <- matrix(
    c(lambda_week[design$week], lambda_week[design$week]),
    ncol = 2
  )
  kappa <- matrix(
    c(kappa_class[design$classroom], kappa_class[design$classroom]),
    ncol = 2
  )
  post = 1 * (design$time == "post")
  cur = 1 * (design$instruction == "curated")

  mu = alpha.mat +
    lambda +
    kappa +
    post * beta1 +
    cur * beta2 +
    post * cur * beta3

  Z = matrix(rnorm(nrow(design) * 2), ncol = 2)

  Y <- Z %*% t(L.u) + mu
  spelling = clip01(Y[, 1])
  phonics = clip01(Y[, 2])

  long_data <- design %>%
    mutate(
      spelling = spelling,
      phonic = phonics
    )

  long_data
}
