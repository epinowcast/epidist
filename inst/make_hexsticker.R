library(hexSticker)
library(sysfonts)
library(ggplot2)

# font setup
font_add_google("Zilla Slab Highlight", "useme")

# make standard plot
outbreak <- simulate_gillespie(seed = 101)

secondary_dist <- data.table(
  meanlog = 1.8, sdlog = 0.5
) |>
  add_natural_scale_mean_sd()

obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$meanlog[[1]],
    sdlog = secondary_dist$sdlog[[1]]
  ) |>
  observe_process()

truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 25)

truncated_obs <- truncated_obs[sample(1:.N, 200, replace = FALSE)]

combined_obs <- combine_obs(truncated_obs, obs)
meanlog <- secondary_dist$meanlog[[1]]
sdlog <- secondary_dist$sdlog[[1]]

plot <- combined_obs |>
  ggplot() +
  aes(x = delay_daily) +
  geom_histogram(aes(y = after_stat(density), fill = obs_at), binwidth = 1, position = "dodge") +
  lims(x = c(0, 18))

if (!missing(meanlog) && !missing(sdlog)) {
  plot <- plot +
    stat_function(
      fun = dlnorm, args = c(meanlog, sdlog), n = 100,
      col = "#696767b1"
    )
}

# strip out most of the background
hex_plot <- plot +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none",
        panel.background = element_blank())

# make and save hexsticker
sticker(
  hex_plot,
  package = "epidist",
  p_size = 23,
  p_color = "#646770",
  s_x = 1,
  s_y = 0.85,
  s_width = 1.3,
  s_height = 0.75,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  url = "epidist.epinowcast.org",
  u_color = "#646770",
  u_size = 3.5
)
