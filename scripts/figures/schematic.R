library(ggplot2)
library(patchwork)

epsilon <- 0.1

generate_base_plot <- function(future = FALSE) {
  future_alpha <- ifelse(future, 0.5, 1)
  base_plot <- ggplot(NULL) +
    geom_point(aes(0, 0), size = 3, shape = 21, fill = "red", stroke = 1) +
    geom_point(aes(1, 0), size = 3, shape = 21, fill = "blue", stroke = 1) +
    geom_point(aes(0, 0.1), size = 3, shape = 21, fill = "red", stroke = 1) +
    geom_point(
      aes(4, 0.1),
      size = 3, shape = 21, fill = "blue", stroke = 1,
      alpha = future_alpha
    ) +
    geom_point(aes(1, 0.2), size = 3, shape = 21, fill = "red", stroke = 1) +
    geom_point(aes(2, 0.2), size = 3, shape = 21, fill = "blue", stroke = 1) +
    geom_point(
      aes(4, 0.3),
      size = 3, shape = 21, fill = "red", stroke = 1,
      alpha = future_alpha
    ) +
    geom_point(
      aes(5, 0.3),
      size = 3, shape = 21, fill = "blue", stroke = 1,
      alpha = future_alpha
    ) +
    scale_x_continuous("Day",
      limits = c(-0.6, 5.5),
      expand = c(0, 0),
      breaks = 0:5,
      labels = c(
        "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
      )
    ) +
    scale_y_continuous("Subject",
      breaks = c(0, 0.1, 0.2, 0.3),
      labels = c(1, 2, 3, 4),
      limits = c(-0.05, 0.38),
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank()
    )
  return(base_plot)
}

add_arrow <- function(plot, y, ends, angle = 30, lty = 1, length = 0.1, ...) {
  plot <- plot +
    geom_segment(
      aes(..., y = y, yend = y),
      arrow = arrow(length = unit(length, "inches"), ends = ends, angle = angle), lwd = 1, lty = lty, col = "black", alpha = 0.95
    )
  return(plot)
}

truncation <- generate_base_plot(future = TRUE) |>
  add_arrow(x = 0.1, y = 0, xend = 0.9, ends = "last") |>
  add_arrow(x = 1.1, y = 0.2, xend = 1.9, ends = "last") |>
  add_arrow(x = 0.1, y = 0.1, xend = 3.9, ends = "last", lty = 2, length = 0) |>
  add_arrow(x = 4.1, y = 0.3, xend = 4.9, ends = "last", lty = 2, length = 0) +
  geom_rect(
    aes(xmin = 3, ymin = -0.05, xmax = 5.5, ymax = 0.35),
    fill = "gray",
    alpha = 0.2
  ) +
  geom_segment(aes(3, -0.05, xend = 3, yend = 0.35), lty = 1, col = "gray50") +
  annotate("text", x = 0.5, y = 0.01, label = "Observed delay", vjust = -0.5) +
  annotate("text", x = 2, y = 0.11, label = "Truncated delay", vjust = -0.5) +
  annotate("text", x = 3, y = 0.35, label = "Observation time", vjust = -0.5) +
  annotate("text", x = 4.5, y = -0.05, label = "Future events", vjust = -1)

censoring <- generate_base_plot(future = FALSE) |>
  add_arrow(x = -0.45, y = 0, xend = -.1, ends = "first", angle = 90) |>
  add_arrow(x = 0.1, y = 0, xend = 0.45, ends = "last", angle = 90) |>
  add_arrow(x = 0.55, y = 0, xend = 0.9, ends = "first", angle = 90) |>
  add_arrow(x = 1.1, y = 0, xend = 1.45, ends = "last", angle = 90) |>
  add_arrow(x = -0.45, y = 0.1, xend = -.1, ends = "first", angle = 90) |>
  add_arrow(x = 0.1, y = 0.1, xend = 2.45, ends = "last", angle = 90) |>
  add_arrow(x = 3.55, y = 0.1, xend = 3.9, ends = "first", angle = 90) |>
  add_arrow(x = 4.1, y = 0.1, xend = 4.45, ends = "last", angle = 90) |>
  add_arrow(x = 0.55, y = 0.2, xend = 0.9, ends = "first", angle = 90) |>
  add_arrow(x = 1.1, y = 0.2, xend = 1.45, ends = "last", angle = 90) |>
  add_arrow(x = 4.55, y = 0.3, xend = 4.9, ends = "first", angle = 90) |>
  add_arrow(x = 5.1, y = 0.3, xend = 5.45, ends = "last", angle = 90) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p <- truncation + censoring +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = "A")

ggsave("figures/schematic.pdf", p, width = 9, height = 4)
