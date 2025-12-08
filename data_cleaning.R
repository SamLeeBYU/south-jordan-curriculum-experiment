library(tidyverse)

red <- read.csv("data/raw_data/red_scores.csv")
blue <- read.csv("data/raw_data/blue_scores.csv")

# Red class curated first
# Blue class curated second
# Red student 17 -> 100 both post assessments
# Blue
#   6 -> 100 for both assessments
#   10, 11 -> 83 for both

### 1. Impute data based on Ella's feedback
red$Unit.12.Spelling.Post[17] = "100.00%"
red$Unit.12.Phonics.Post[17] = "100.00%"

blue$Unit.11.Spelling.Post[6] = 100
blue$Unit.11.Phonics.Post[6] = 100

blue$Unit.12.Phonics.Post[10] = 83
blue$Unit.12.Phonics.Post[11] = 83

### 2. Drop rows where Student.Number is NA ----
red <- red %>% filter(!is.na(Student.Number))
blue <- blue %>% filter(!is.na(Student.Number))

### 3. Add classroom labels ----
red <- red %>% mutate(classroom = "red")
blue <- blue %>% mutate(classroom = "blue")

### 4. Make score columns numeric ----
pct_to_num <- function(x) {
  x %>% str_replace("%", "") %>% as.numeric()
}

red <- red %>%
  mutate(across(starts_with("Unit"), pct_to_num))

blue <- blue %>%
  mutate(across(starts_with("Unit"), as.numeric))


### 5. Combine into one dataset ----
all <- bind_rows(red, blue)

### 6. Pivot to long format ----
long <- all %>%
  pivot_longer(
    cols = matches("^Unit\\.\\d+"),
    names_to = c("unit", "test", "time"),
    names_pattern = "^(Unit\\.\\d+)\\.(Spelling|Phonics)\\.(Pre|Post)$",
    values_to = "score"
  )

### 7. Recode variables ----
long <- long %>%
  mutate(
    id = Student.Number,
    week = ifelse(unit == "Unit.11", 1L, 2L),
    time = factor(time, levels = c("Pre", "Post")),
    test = tolower(test), # "Spelling" -> "spelling", "Phonics" -> "phonics"
    classroom = factor(classroom)
  )

### 8. Assign instruction type based on classroom × week ----
instruction_fun <- function(classroom, week) {
  if (classroom == "blue" && week == 1) {
    return("traditional")
  }
  if (classroom == "blue" && week == 2) {
    return("curated")
  }
  if (classroom == "red" && week == 1) {
    return("curated")
  }
  if (classroom == "red" && week == 2) return("traditional")
}

long <- long %>%
  rowwise() %>%
  mutate(instruction = instruction_fun(classroom, week)) %>%
  ungroup() %>%
  mutate(
    instruction = factor(instruction, levels = c("traditional", "curated"))
  )

### 9. Final cleanup ----
final_data <- long %>%
  dplyr::select(id, Age, classroom, week, time, instruction, test, score) %>%
  arrange(id, week, test, time)

glimpse(final_data)
saveRDS(final_data, "data/final_project_scores.rds")
