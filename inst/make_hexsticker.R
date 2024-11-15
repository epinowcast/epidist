library(hexSticker)
library(ggplot2)
library(dplyr)
library(magick)

# Make standard plot
outbreak <- simulate_gillespie(seed = 101)

secondary_dist <- data.frame(mu = 1.8, sigma = 0.5)
class(secondary_dist) <- c("lognormal_samples", class(secondary_dist))
secondary_dist <- add_mean_sd(secondary_dist)

obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$mu[[1]],
    sdlog = secondary_dist$sigma[[1]]
  ) |>
  mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_daily + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_daily + 1,
    delay_daily = .data$stime_lwr - .data$ptime_lwr
  )

obs_time <- 25
truncated_obs <- obs |>
  filter(.data$stime_upr <= obs_time) |>
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
    binwidth = 1,
    position = "dodge"
  ) +
  lims(x = c(0, 18)) +
  stat_function(
    fun = dlnorm,
    args = c(meanlog, sdlog),
    n = 100,
    col = "#646770"
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
  p_size = 76,
  p_x = 1.35,
  p_y = 1.1,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

# Make outside of hex sticker transparent
p <- image_read(file.path("man", "figures", "logo.png"))
fuzz <- 50

pp <- p |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = "+1+1"
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+1")
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+1", "+", image_info(p)$height - 1)
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+", image_info(p)$height - 1)
  )

image_write(image = pp, path = file.path("man", "figures", "logo.png"))

usethis::use_logo(file.path("man", "figures", "logo.png"))
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
