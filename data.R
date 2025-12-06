library(dplyr)

ellas.class <- readRDS("data/final_project_scores.rds") %>%
  pivot_wider(names_from = "test", values_from = "score")
