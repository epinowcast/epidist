# Load packages
library(here) # for finding files
library(data.table) # for general data manipulation
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots

# Load available models
models <- fread(here("data/meta/models.csv")) |>
  DT(, model := factor(model))

# Define a function to read diagnostics from a model
read_diagnostics <- function(target_model) {
  # Load diagnostics
  diagnostics <- fread(
    here(
      "data", "diagnostics",
      paste0(models[model %in% target_model, in_code], ".csv")
    )
  )

  # Add model name to each row
  diagnostics <- diagnostics |>
    DT(, model := model)

  return(diagnostics)
}

# Load diagnostics for each model and combine
diagnostics <- map_dfr(models$model, read_diagnostics)


# Metrics of interest
# - run_time
# - rhat (per with rhat > 1.05)?
# - per_divergent transitions
# - per_at _max_tree_depth and max_tree_depth == 10 (or whatever max we set)
# - Effective sample size (not currently collected). Both bulk and tail.

# Strata of interest
# - model
# - data type (outbreak/exponential/case study)
# - scenario/observation day
# - distribution (short, medium, long)

# Variables to control for
# - sample size - just focus on 200 for now and perhaps include diagnostics for other samples in the the SI? # nolint
# - observation type (i.e real-time or retrospective). Suggest we drop retrospective and focus on real-time. # nolint

# Plot types
# - violin
# - density
# - mean as point/line?

# Plot Percentage with an Rhat greater than 1.05
# - Line and point plot
# - X axis: Models
# - Y axis: Percentage with Rhat > 1.05
# - Colour: Distribution
# - 