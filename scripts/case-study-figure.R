# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots

# Load case study data
case_study_obs <- fread("data/scenarios/ebola_case_study.csv")

# Load available models
models <- fread("data/meta/models.csv")

# Function to read each arrow dataset, filter for the case study
# and attach the model name
read_case_study <- function(target_model) {
  # Load posterior samples as an arrow dataset
  dataset <- open_dataset(
    here("data", "posteriors", models[model %in% target_model, in_code])
  )

  # Filter for the case study related samples
  # Add model name to each row
  samples <- dataset |>
    filter(data_type %in% "ebola_case_study") |>
    mutate(model = target_model) |>
    collect()

  return(samples)
}

# Load case study samples for each model and combine
cs_samples <- map_dfr(models$model, read_case_study)

# Get observation times
obs_times <- cs_samples |>
  DT(, unique(scenario)) |>
  gsub(" days", x = _, "") |>
  as.numeric() |>
  (\(x) x[order(x)])()

# Make inidividual plots

# Plot observed cases by observation window
truncated_cs_obs_by_window <- construct_cases_by_obs_window(
  case_study_obs, windows = obs_times, obs_type = "stime"
)

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window)

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_cs_obs <-  obs_times |>
  map_dfr(~ filter_obs_by_obs_time(case_study_obs, obs_time = .x)) |>
  combine_obs(case_study_obs) |>
  DT(, type := "Real-time")

retro_cs_obs <- obs_times |>
  map_dfr(~ filter_obs_by_ptime(case_study_obs, obs_time = .x)) |>
  combine_obs(case_study_obs) |>
  DT(, type := "Retrospective")

combined_cs_obs <- rbind(truncated_cs_obs, retro_cs_obs)

empirical_pmf_plot <- combined_cs_obs |>
  DT(delay_daily <= 20) |>
  plot_empirical_delay() +
  facet_wrap(vars(type), ncol = 1)

# Summarise draws

# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
paramter_density_plot <- cs_samples |>
  draws_to_long() |>
  DT(value <= 10) |>
  DT(value >= -10) |>
  DT(parameter %in% c("mean", "sd")) |>
  DT(, scenario := factor(scenario, levels = paste0(obs_times, " days"))) |>
  ggplot() +
    aes(x = value, y = model, fill = obs_type) +
    ggridges::geom_density_ridges(
      scale = 1.5, alpha = 0.8
    ) +
  theme_bw() +
  facet_grid(vars(parameter), vars(scenario), scales = "free_x") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    y = "Model", x = "Distribution summary parameter values"
  ) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Observation type"))

# Combine plots into a single figure
# - plot observations by estimation time (plot_cases_by_obs_window())
# - plot empirical discretised PMF for each observation window (plot_empirical_pmf()).
# - plot delay summary parameters for each observation window, method and type (i.e real-time and retrospective).
# Save combined plots
