source("themes.R")
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

ellas.class.dgp <- ellas.class %>%
  ggplot(aes(
    x = spelling,
    y = phonics,
    color = classroom,
    shape = instruction
  )) +
  geom_line(
    aes(group = interaction(id, week)),
    color = "#DDD",
    linewidth = 0.15,
    show.legend = F
  ) +
  geom_point() +
  scale_color_paper() +
  labs(
    x = "Spelling",
    y = "Phonics",
    shape = "Instruction",
    color = "Class"
  ) +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )

ggsave(
  "Figures/dgp.png",
  ellas.class.dgp,
  width = 5,
  height = 2.8,
  dpi = 900
)

##### Simulation Study Results #####

rr <- readRDS("rejection-rates-sim.RDS")
rr.plt <- ggplot(rr, aes(spelling, phonics, fill = phatFE)) +
  geom_raster(interpolate = F) +
  geom_text(aes(label = sprintf("%.2f", phatFE)), color = "white", size = 16) +
  scale_fill_viridis_c(option = "turbo") +
  coord_equal() +
  labs(fill = "Rejection Rates", x = "Spelling Effect", y = "Phonics Effect") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )
ggsave(
  "Figures/rr.png",
  rr.plt,
  width = 4,
  height = 4,
  dpi = 900
)


##### BAYESIAN ESTIMATION #####

allsamps <- readRDS("mcmc-samples.rds")

curriculum.effects <- allsamps[, c(6, 12)]
x <- allsamps[, 6] #Spelling
y <- allsamps[, 12] #Phonics
# colMeans(curriculum.effects)
# mean(curriculum.effects[,1] > 0 | curriculum.effects[,2] > 0)

dens <- kde2d(x, y, n = 100)
df <- with(
  dens,
  expand.grid(x = x, y = y) |>
    mutate(z = as.vector(z))
)

ggplot(df, aes(x, y, fill = z)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "turbo") +
  coord_equal() +
  labs(fill = "Density", x = "Spelling Effect", y = "Phonics Effect") +
  theme_paper()

### EDA ###
final_data <- readRDS("data/final_project_scores.rds")
post.scores.by.instruction <- final_data %>%
  group_by(instruction, test, time) %>%
  summarize(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(time, mean_score, group = instruction, color = instruction)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ test) +
  labs(title = "Pre/Post Scores by Instruction Type",
       y = "Mean Score (%)") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )
ggsave(
  "Figures/post_scores_by_instruction.png",
  post.scores.by.instruction,
  width = 5,
  height = 2.8,
  dpi = 900
)

### Spaghetti Plot ###
spaghetti.plot <- final_data %>%
  ggplot(aes(time, score, group = id, color = classroom)) +
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.6) +
  facet_grid(test ~ week) +
  labs(title = "Individual Pre/Post Trajectories by Classroom and Week") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )
ggsave(
  "Figures/spaghetti_plot.png",
  spaghetti.plot,
  width = 5,
  height = 2.8,
  dpi = 900
)

### Gain score distributions ###
gain_data <- final_data %>%
  pivot_wider(names_from = time, values_from = score) %>%
  mutate(gain = Post - Pre)

gain.plot <- ggplot(gain_data, aes(x = instruction, y = gain, fill = instruction)) +
  geom_boxplot() +
  facet_wrap(~ test) +
  labs(title = "Gain Scores by Instruction Type", y = "Gain (Post - Pre)") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )
ggsave(
  "Figures/gain_plot.png",
  gain.plot,
  width = 5,
  height = 2.8,
  dpi = 900
)


### Classroom vs Instruction Effect ###
class.vs.instruction <- final_data %>%
  group_by(classroom, week, instruction, test) %>%
  summarize(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(week, mean_score, color = instruction, group = instruction)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ test) +
  labs(title = "Cross-over Design: Week x Instruction Effects") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )

ggsave(
  "Figures/class_vs_instruction.png",
  class.vs.instruction,
  width = 5,
  height = 2.8,
  dpi = 900
)

### Scatterplot: spelling v phonics multivariate view ###
wide_post <- final_data %>%
  filter(time == "Post") %>%              # only post scores
  dplyr::select(id, classroom, week, instruction, test, score) %>%
  pivot_wider(
    names_from = test,                    # "spelling", "phonics"
    values_from = score
  )

spelling.v.phonics <- wide_post %>%
  ggplot(aes(spelling, phonics, color = instruction)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Relationship Between Spelling and Phonics (Post Scores)") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )

ggsave(
  "Figures/spelling_v_phonics.png",
  spelling.v.phonics,
  width = 5,
  height = 2.8,
  dpi = 900
)

### Heat map of mean scores ###
heat.map <- final_data %>%
  group_by(week, time, test) %>%
  summarize(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(time, test, fill = mean_score)) +
  geom_tile() +
  facet_wrap(~ week) +
  scale_fill_viridis_c() +
  labs(title = "Mean Scores by Week x Time x Test") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )

ggsave(
  "Figures/heat_map_mean_scores.png",
  heat.map,
  width = 5,
  height = 2.8,
  dpi = 900
)

### Effect size plot ###
gain_summary <- gain_data %>%
  group_by(test, instruction) %>%
  summarize(mean_gain = mean(gain), .groups = "drop") %>%
  pivot_wider(names_from = instruction, values_from = mean_gain) %>%
  mutate(effect = curated - traditional)

effect.size.plot <- ggplot(gain_summary, aes(test, effect)) +
  geom_col(fill = "steelblue") +
  labs(title = "Effect Size of Curated Instruction",
       y = "Curated - Traditional Gain") +
  theme_paper(base_size = 10, text_size = 64) +
  theme(
    legend.title = element_text(size = 64),
    legend.text = element_text(size = 64)
  )

ggsave(
  "Figures/effect_size_plot.png",
  effect.size.plot,
  width = 5,
  height = 2.8,
  dpi = 900
)

