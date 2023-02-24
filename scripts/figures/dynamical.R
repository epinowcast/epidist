# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation
library(forcats) # manipulate factors
library(scoringutils) # for scoring forecasts/posterior predictions
load(here("scripts/simulations", "dynamical_simulation.rda"))

# Load case study data
outbreak_obs <- fread(here("data/scenarios/outbreak-simulation.csv"))

# Load distributions
distributions <- fread(here("data/meta/distributions.csv")) |>
  DT(scenario == "long")

outbreak_long <- outbreak_obs |>
  DT(distribution == "long")

obs_window <- 60

truncated_obs <- outbreak_long |>
  filter_obs_by_obs_time(obs_time = obs_window)

backward <- truncated_obs |>
  copy() |>
  DT(, .(mean = mean(delay_daily)), by = stime_daily)

g1 <- ggplot(backward_simulation_summ) +
  geom_point(data = backward, aes(stime_daily, mean)) +
  geom_ribbon(
    aes(time, ymin = q5, ymax = q95, fill = factor(obs_t)), alpha = 0.3
  ) +
  geom_line(aes(time, mean, col = factor(obs_t))) +
  scale_x_continuous("Secondary event time (days)") +
  scale_y_continuous("Mean backward delay (days)") +
  scale_fill_viridis_d("Time") +
  scale_color_viridis_d("Time") +
  facet_wrap(~type, nrow = 2) +
  theme_bw() +
  theme(legend.position = "bottom")

g2 <- backward_simulation_draw |>
  make_relative_to_truth(
    distributions |>
      draws_to_long() |>
      DT(, variable := str_to_sentence(parameter)) ,
    by = "variable"
  ) |>
  filter(rel_value < 2) |>
  ggplot() +
  ggridges::geom_density_ridges(
    aes(x = rel_value, y = factor(obs_t, levels = c(60, 45, 30, 15)),
        fill = type
    ),
    scale = 1.5, quantile_lines = TRUE, alpha = 0.8,
    quantiles = c(0.05, 0.35, 0.65, 0.95)
  ) +
  geom_vline(xintercept = 1, linetype = 2, size = 1.05, alpha = 0.8) +
  facet_wrap(vars(variable), scales = "free_x") +
  scale_fill_brewer(palette = "Dark2") +
  guides(
    fill = guide_legend(title = "Estimation method", nrow = 2),
    col = guide_none()
  ) +
  labs(
    y = "Observation day", x = "Relative to ground truth"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

gcomb <- g1 + g2 +
  plot_layout(nrow = 2, height = c(3, 1)) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Save combined plots
ggsave(
  here("figures", "dynamical.png"), gcomb,
  height = 12, width = 8, dpi = 330
)
