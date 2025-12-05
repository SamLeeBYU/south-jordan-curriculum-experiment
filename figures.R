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
