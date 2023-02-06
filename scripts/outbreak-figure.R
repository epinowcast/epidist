# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(ggh4x) # for customised facets

#TODO: Do we want to capitalise facet titles?

# Load case study data
outbreak_obs <- fread(here("data/scenarios/outbreak-simulation.csv"))

# Load available models
models <- fread(here("data/meta/models.csv"))

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
obs_times <- fread(here("data/meta/outbreak_estimation_times.csv")) |>
  DT(, time)

# Load distributions
distributions <- fread(here("data/meta/distributions.csv"))

# Make inidividual plots

# Plot observed cases by observation window
# Observe at 100 days in addition to estimation times
truncated_cs_obs_by_window <- outbreak_obs |>
  split(by = "distribution") |>
  map_dfr(
    ~ construct_cases_by_obs_window(
      ., windows = obs_times, obs_type = "stime", upper_window = 100
    ),
    .id = "distribution"
  ) |>
  DT(,
    distribution := factor(distribution, levels = c("short", "medium", "long"))
  )

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window) +
  facet_wrap(vars(distribution), ncol = 1)

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_outbreak_obs <-  c(obs_times, 100) |>
  map_dfr(~ filter_obs_by_obs_time(outbreak_obs, obs_time = .x))

# TODO: Add Simulation delay to PMF plot
# TODO: Get ordering of observation windows to match obs plot
empirical_pmf_plot <- truncated_outbreak_obs |>
  reverse_obs_at() |>
  DT(delay_daily <= 20) |>
  plot_empirical_delay() +
  facet_wrap(vars(distribution), ncol = 1)

# Summarise draws

# TODO: Order model names in order of expected accuracy
# TODO: Plotting error or is mean recovery really so bad?
# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- o_samples |>
  draws_to_long() |>
  DT(parameter %in% c("mean", "sd")) |>
  make_relative_to_truth(
    draws_to_long(distributions) |>
    setnames("scenario", "distribution"),
    by = c("parameter", "distribution")
  ) |>
  DT(value <= 5) |>
  DT(value >= 0.05) |>
  DT(,
    distribution := factor(distribution, levels = c("short", "medium", "long"))
  ) |>
  plot_relative_recovery(fill = scenario) +
  facet_grid(
    vars(distribution), vars(parameter), scales = "free_x"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    y = "Model", x = "Relative to ground truth"
  ) +
  theme(legend.position = "bottom")
