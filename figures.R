source("themes.R")
source("data.R")

ellas.class.dgp <- ellas.class %>%
  ggplot(aes(
    x = spelling,
    y = phonics,
    color = classroom,
    shape = instruction
  )) +
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
