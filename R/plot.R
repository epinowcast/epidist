plot_relative_recovery <- function(relative_data, alpha = 0.8,
                                   quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  relative_data |>
    ggplot() +
    aes(x = rel_value, y = model, ...) +
    ggridges::geom_density_ridges(
      scale = 1.5, alpha = alpha, quantile_lines = TRUE,
      quantiles = quantiles
    ) +
    scale_x_continuous(
      trans = "log", limits = c(NA, NA),
      labels = ~ scales::comma(.x, accuracy = 0.1),
    ) +
    geom_vline(xintercept = 1, linetype = 2, size = 1.05, alpha = 0.8) +
    theme_bw() +
    labs(x = "Relative value")
}
