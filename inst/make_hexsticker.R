library(hexSticker)
library(sysfonts)
library(ggplot2)
library(dplyr)

# font setup
font_add_google("Zilla Slab Highlight", "useme")

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
  mutate(obs, obs_at = max(stime_daily))
) |>
  mutate(obs_at = factor(obs_at))

meanlog <- secondary_dist$mu[[1]]
sdlog <- secondary_dist$sigma[[1]]

hex_plot <- combined_obs |>
  ggplot() +
  aes(x = delay_daily, fill = obs_at) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1, position = "dodge"
  ) +
  lims(x = c(0, 18)) +
  stat_function(
    fun = dlnorm, args = c(meanlog, sdlog), n = 100,
    col = "#696767b1"
  ) +
  scale_fill_brewer(palette = "Set2", direction = 1) +
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
  package = "epidist",
  p_size = 23,
  p_color = "#646770",
  p_x = 1.3,
  p_y = 1.15,
  s_x = 0.85,
  s_y = 1,
  s_width = 1.2,
  s_height = 1.2,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png")
)
