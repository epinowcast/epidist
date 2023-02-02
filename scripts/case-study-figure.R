# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting

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



# Make inidividual plots

# Combine plots

# - plot observations by estimation time (plot_cases_by_obs_window())
# - plot empirical discretised PMF for each observation window (plot_empirical_pmf()).
# - plot delay summary parameters for each observation window, method and type (i.e real-time and retrospective).
# Save combined plots