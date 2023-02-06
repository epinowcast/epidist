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
case_study_obs <- fread(here("data/scenarios/ebola_case_study.csv"))

# Load available models
models <- fread(here("data/meta/models.csv"))

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


# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- cs_samples |>
  draws_to_long() |>
  DT(value <= 10) |>
  DT(value >= -10) |>
  DT(parameter %in% c("mean", "sd")) |>
  DT(, scenario := gsub(" days", x = scenario, replacement = "") |>
    factor(levels = obs_times) |>
    fct_rev()
  ) |>
  DT(, obs_type := str_to_sentence(obs_type)) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  DT(, model := factor(model, levels = models$model)) |>
  plot_recovery(y = scenario, fill = obs_type) +
  facet_grid(
    vars(model), vars(parameter),
    labeller = label_wrap_gen(multi_line = TRUE),
    scales = "free_x"
    ) +
  scale_fill_brewer(palette = "Dark2") +
  guides(
    fill = guide_legend(title = "Estimation method"), col = guide_none()
  ) +
  labs(
    y = "Observation day", x = "Parameter estimate"
  ) +
  theme(legend.position = "bottom")

# Combine plots
case_study_plot <- obs_plot / (
  (empirical_pmf_plot + guides(fill = guide_none())) +
  parameter_density_plot +
  plot_layout(width = c(1, 2))
) +
plot_annotation(tag_levels = "A") +
plot_layout(guides = "collect", height = c(1, 4)) &
theme(legend.position = "bottom")


# Save combined plots
ggsave(
  here("figures", "case_study.png"), case_study_plot,
  height = 20, width = 16, dpi = 330
)