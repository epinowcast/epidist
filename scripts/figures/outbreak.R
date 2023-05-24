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

# Load case study data
outbreak_obs <- fread(here("data/scenarios/outbreak-simulation.csv"))

# Load available models
models <- fread(here("data/meta/models.csv")) |>
  DT(, model := factor(model))

# Function to read each arrow dataset, filter for the case study
# and attach the model name
read_outbreak <- function(target_model) {
  # Load posterior samples as an arrow dataset
  dataset <- open_dataset(
    here("data", "posteriors", models[model %in% target_model, in_code])
  )

  # Filter for the case study related samples
  # Add model name to each row
  samples <- dataset |>
    filter(data_type %in% "outbreak") |>
    mutate(model = target_model) |>
    collect()

  return(samples)
}

# Load outbreak samples for each model and combine
o_samples <- map_dfr(models$model, read_outbreak)

# Get observation times
outbreak_estimation_times <- fread(
  here("data/meta/outbreak_estimation_times.csv")
) |>
  DT(, scenario_days := paste0(
    scenario, " (day ", time, ")"
  )) |>
  DT(, scenario := factor(scenario)) |>
  DT(, scenario_days := factor(scenario_days))

obs_times <- outbreak_estimation_times |>
  DT(, time)

# Load distributions
distributions <- fread(here("data/meta/distributions.csv")) |>
  DT(, distribution_stat := paste0(
    str_to_sentence(scenario),
    " (mean: ", round(mean, 1),
    ", sd: ", round(sd, 1), ")"
  )) |>
  DT(, distribution_stat := factor(distribution_stat) |>
    fct_rev()) |>
  DT(, pmf := map2(
    meanlog, sdlog,
    ~ dlnorm(seq(0, 20, by = 0.1), meanlog = .x, sdlog = .y)
  ))

# Make inidividual plots

# Plot observed cases by observation window
# Observe at 100 days in addition to estimation times
truncated_cs_obs_by_window <- outbreak_obs |>
  split(by = "distribution") |>
  map_dfr(
    ~ construct_cases_by_obs_window(
      .,
      windows = obs_times, obs_type = "stime", upper_window = 100
    ),
    .id = "distribution"
  ) |>
  DT(
    distributions[, .(distribution = scenario, distribution_stat)],
    on = "distribution"
  )

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window) +
  facet_wrap(vars(distribution_stat), nrow = 1) +
  guides(
    fill = guide_legend(title = "Observation day", reverse = TRUE, nrow = 2),
    color = guide_legend(title = "Observation day", reverse = TRUE, nrow = 2)
  )

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_outbreak_obs <- c(obs_times, 100) |>
  map_dfr(~ filter_obs_by_obs_time(outbreak_obs, obs_time = .x))

empirical_pmf_plot <- truncated_outbreak_obs |>
  reverse_obs_at() |>
  DT(, obs_at := factor(obs_at, levels = rev(unique(as.character(obs_at))))) |>
  DT(delay_daily <= 20) |>
  DT(
    distributions[, .(distribution = scenario, distribution_stat)],
    on = "distribution"
  ) |>
  plot_empirical_delay() +
  geom_line(
    data = distributions |>
      DT(,
        .(pmf = unlist(pmf), delay_daily = seq(0, 20, by = 0.1)),
        by = "distribution_stat"
      ),
    aes(y = pmf, fill = NULL), col = "black"
  ) +
  facet_wrap(vars(distribution_stat), ncol = 1)

# Make a clean samples data.frame for plotting
clean_o_samples <- o_samples |>
  draws_to_long() |>
  DT(parameter %in% c("mean", "sd")) |>
  make_relative_to_truth(
    draws_to_long(distributions) |>
      setnames("scenario", "distribution"),
    by = c("parameter", "distribution")
  ) |>
  merge(outbreak_estimation_times, by = "scenario") |>
  DT(, distribution_stat := distribution_stat |>
    str_to_sentence() |>
    factor() |>
    fct_rev()) |>
  DT(, time := factor(time) |> fct_rev()) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  DT(, model := factor(model, levels = models$model))

# Summarise draws

# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- clean_o_samples |>
  DT(sample_size == 400) |>
  DT(rel_value <= 2) |>
  DT(rel_value >= 0.1) |>
  plot_relative_recovery(y = time, fill = distribution_stat) +
  facet_grid(
    vars(model), vars(parameter),
    labeller = label_wrap_gen(multi_line = TRUE),
    scales = "free_x"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  guides(
    fill = guide_legend(title = "Distribution", nrow = 2),
    col = guide_none()
  ) +
  labs(
    y = "Observation day", x = "Relative to ground truth"
  ) +
  theme(legend.position = "bottom")

# Score models against known values
scores <- clean_o_samples |>
  DT(sample_size == 400) |>
  copy() |>
  DT(,
    sample := 1:.N,
    keyby = c("model", "scenario", "parameter", "distribution_stat", "time")
  ) |>
  DT(, pmf := NULL) |>
  DT(, true_value := 0) |>
  DT(, prediction := log(rel_value)) |>
  DT(
    ,
    c(
      "model", "scenario", "parameter", "distribution_stat", "time", "sample",
      "prediction", "true_value"
    )
  ) |>
  score()

# overall scores
overall_scores <- scores |>
  summarise_scores(by = "model")

# scores by distribution and time
scores_by_distribution <- scores |>
  summarise_scores(by = c("model", "distribution_stat", "time", "parameter"))

# plot scores
scores_plot <- scores_by_distribution |>
  DT(, time := time |>
    fct_rev()) |>
  DT(, model := model |>
    fct_rev()) |>
  ggplot() +
  aes(
    x = crps, y = model, col = distribution_stat, size = time,
    shape = parameter
  ) +
  geom_point(position = position_jitter(width = 0, height = 0.2), alpha = 0.6) +
  geom_point(
    data = overall_scores |>
      DT(, model := model |>
        fct_rev()),
    col = "black", shape = 5, size = 4, alpha = 1
  ) +
  theme_bw() +
  guides(
    col = guide_none(),
    size = guide_legend(title = "Observation day", nrow = 2),
    shape = guide_legend(title = "Parameter", nrow = 2)
  ) +
  scale_colour_brewer(palette = "Dark2") +
  # add a linebreak to y axis labels
  scale_y_discrete(labels = (\(x) str_wrap(x, width = 30))) +
  scale_x_log10() +
  theme(legend.position = "bottom") +
  labs(y = "Model", x = "Relative CRPS")

# Combine plots
outbreak_plot <- obs_plot /
  (
    (
      (
        (empirical_pmf_plot + guides(fill = guide_none())) /
          scores_plot
      ) + plot_layout(height = c(2, 1)) |
        parameter_density_plot
    ) +
      plot_layout(width = c(1, 2))
  ) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", height = c(1, 5)) &
  theme(legend.position = "bottom")
# break legends into two columns


# Save combined plots
ggsave(
  here("figures", "outbreak.pdf"), outbreak_plot,
  height = 24, width = 16, dpi = 330
)
