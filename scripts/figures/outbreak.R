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
    )
  ) |>
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
    fct_rev()
  )

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
  DT(
    distributions[, .(distribution = scenario, distribution_stat)],
    on = "distribution"
  )

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window) +
  facet_wrap(vars(distribution_stat), nrow = 1)

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_outbreak_obs <-  c(obs_times, 100) |>
  map_dfr(~ filter_obs_by_obs_time(outbreak_obs, obs_time = .x))

# TODO: Add Simulation delay to PMF plot
empirical_pmf_plot <- truncated_outbreak_obs |>
  reverse_obs_at() |>
  DT(, obs_at := factor(obs_at, levels = rev(unique(as.character(obs_at))))) |>
  DT(delay_daily <= 20) |>
  DT(
    distributions[, .(distribution = scenario, distribution_stat)],
    on = "distribution"
  ) |>
  plot_empirical_delay() +
  facet_wrap(vars(distribution_stat), ncol = 1)

# Summarise draws

# TODO: Plotting error or is mean recovery really so bad? It looks to me like an issue with the simulation grid rather than in this script # nolint
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
  merge(outbreak_estimation_times, by = "scenario") |>
  DT(,
      c("distribution_stat",  "scenario_days") :=
        map(.SD, ~ . |>
          str_to_sentence() |>
          factor() |>
          fct_rev()
        ),
      .SDcols = c("distribution_stat",  "scenario_days")
  ) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  DT(, model := factor(model, levels = models$model)) |>
  plot_relative_recovery(y = scenario_days, fill = distribution_stat) +
  facet_grid(
    vars(model), vars(parameter),
    labeller = label_wrap_gen(multi_line = TRUE),
    scales = "free_x"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(title = "Distribution"), col = guide_none()) +
  labs(
    y = "Observation time", x = "Relative to ground truth"
  ) +
  theme(legend.position = "bottom")


# Combine plots
outbreak_plot <- obs_plot / (
  (empirical_pmf_plot + guides(fill = guide_none())) +
  parameter_density_plot +
  plot_layout(width = c(1, 2))
) +
plot_annotation(tag_levels = "A") +
plot_layout(guides = "collect", height = c(1, 4)) &
theme(legend.position = "bottom")


# Save combined plots
ggsave(
  here("figures", "outbreak.png"), outbreak_plot,
  height = 20, width = 16, dpi = 330
)