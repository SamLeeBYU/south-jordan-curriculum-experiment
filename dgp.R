library(dplyr)
library(tidyr)

#This DGP was originally written by Gavin Hatch, recoded by Sam Lee

generate_classroom_data <- function(
  n_per_class = 18,
  classes = c("blue", "red"),
  ability_mean = 70,
  ability_sd = 6,
  test_effect = c(spelling = 2, phonics = -2),
  class_sd = 3,
  week_effect = c(`1` = 0, `2` = 1),
  gain_base = 4,
  gain_curated_bonus = 3,
  resid_sd_pre = 4,
  resid_sd_post = 3,
  clip_lo = 0,
  clip_hi = 100,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ----------------------------
  # Helper: instruction by week/class (fixed cross-over)
  # ----------------------------
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

  # ----------------------------
  # Basic design
  # ----------------------------
  students <- data.frame(
    id = 1:(length(classes) * n_per_class),
    classroom = rep(classes, each = n_per_class)
  )

  design <- expand.grid(
    id = students$id,
    week = 1:2,
    test = c("spelling", "phonics"),
    time = c("pre", "post"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  design <- design %>%
    left_join(students, by = "id") %>%
    mutate(
      instruction = mapply(instruction_fun, classroom, week),
      week = factor(week),
      test = factor(test),
      time = factor(time, levels = c("pre", "post")),
      instruction = factor(instruction, levels = c("traditional", "curated")),
      classroom = factor(classroom)
    )

  # ----------------------------
  # Simulate abilities & random effects
  # ----------------------------
  ability_student <- rnorm(nrow(students), mean = ability_mean, sd = ability_sd)
  names(ability_student) <- students$id

  class_effect <- rnorm(length(classes), mean = 0, sd = class_sd)
  names(class_effect) <- classes

  clip01 <- function(x, lo = clip_lo, hi = clip_hi) pmin(pmax(x, lo), hi)

  # ----------------------------
  # Generate scores
  # ----------------------------
  long_data <- design %>%
    rowwise() %>%
    mutate(
      # baseline mean for this student's test/week
      mu_pre = ability_student[as.character(id)] +
        test_effect[[as.character(test)]] +
        class_effect[[as.character(classroom)]] +
        week_effect[[as.character(week)]],

      # actual pre-score
      score_pre = rnorm(1, mean = mu_pre, sd = resid_sd_pre),

      # gain depends on instruction type
      gain_mean = gain_base +
        ifelse(instruction == "curated", gain_curated_bonus, 0),
      gain = rnorm(1, mean = gain_mean, sd = resid_sd_post),

      score = ifelse(time == "pre", score_pre, score_pre + gain),
      score = clip01(score)
    ) %>%
    ungroup() %>%
    select(id, classroom, week, test, time, instruction, score)

  long_data
}

ellas.class <- generate_classroom_data(seed = 666)
