library(hexSticker)
library(ggplot2)
library(dplyr)

# make standard plot
outbreak <- simulate_gillespie(seed = 101)

secondary_dist <- data.frame(mu = 1.8, sigma = 0.5)
class(secondary_dist) <- c("lognormal_samples", class(secondary_dist))
secondary_dist <- add_mean_sd(secondary_dist)

obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$mu[[1]],
    sdlog = secondary_dist$sigma[[1]]
  ) |>
  observe_process()

truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 25) |>
  slice_sample(n = 200, replace = FALSE)

combined_obs <- bind_rows(
  truncated_obs,
  mutate(obs, obs_time = max(stime_daily))
) |>
  mutate(obs_time = factor(obs_time))

meanlog <- secondary_dist$mu[[1]]
sdlog <- secondary_dist$sigma[[1]]

hex_plot <- combined_obs |>
  ggplot() +
  aes(x = delay_daily, fill = obs_time) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1, position = "dodge"
  ) +
  lims(x = c(0, 18)) +
  stat_function(
    fun = dlnorm, args = c(meanlog, sdlog), n = 100,
    col = "#696767b1"
  ) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

# Make and save hexsticker
sticker(
  hex_plot,
  s_x = 1,
  s_y = 0.85,
  s_width = 2.4,
  s_height = 1.9,
  package = "epidist",
  p_color = "#646770",
  p_size = 42,
  p_x = 1.3,
  p_y = 1.1,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 600,
  white_around_sticker = TRUE
)
