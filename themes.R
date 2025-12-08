library(ggplot2)
library(patchwork)
library(sysfonts)

font_add("cm", regular = "fonts/cmunrm.ttf")
showtext::showtext_auto()

theme_paper <- function(base_size = 14, text_size = 14, base_family = "cm") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Text
      plot.title = element_text(
        face = "bold",
        size = text_size + 2,
        hjust = 0.5,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = text_size,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        size = text_size - 2,
        hjust = 1,
        margin = margin(t = 6)
      ),
      axis.title.x = element_text(size = text_size, margin = margin(t = 8)),
      axis.title.y = element_text(
        size = text_size,
        angle = 90,
        margin = margin(r = 8)
      ),
      axis.text = element_text(size = text_size - 1),

      # Panel & grid
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      #panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
      panel.border = element_rect(fill = NA, color = "grey60", linewidth = 0.4),

      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = base_size - 1, face = "bold"),
      legend.text = element_text(size = base_size - 2),
      legend.key.width = unit(1.2, "lines"),

      # Strips (for facets)
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold", size = base_size - 1),

      # Margins
      plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
    )
}

paper_palette <- c(
  "#5e57be",
  "#d94202",
  "#1b9e77",
  "#e7298a",
  "#66a61e",
  "#e6ab02"
)

scale_color_paper <- function(...) {
  scale_color_manual(values = paper_palette, ...)
}

scale_fill_paper <- function(...) {
  scale_fill_manual(values = paper_palette, ...)
}

scale_fill_gradient2_paper <- function(
  low = "#7570b3",
  mid = "white",
  high = "#d95f02",
  midpoint = 0,
  ...
) {
  scale_fill_gradient2(
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    ...
  )
}
