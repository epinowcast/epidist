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
    DT(, model := target_model)

  return(diagnostics)
}

# Load diagnostics for each model and combine
diagnostics <- map_dfr(models$model, read_diagnostics)


# Add labels
clean_diagnostics <- diagnostics |>
  copy() |>
  DT(, model := factor(model, levels = rev(models$model))) |>
  setkey(sample_size) |>
  DT(,
   sample_size := factor(
    sample_size, levels = unique(sample_size[order(sample_size)])
    )
  ) |>
  DT(, data_type := data_type |>
    gsub("_", " ", x = _)  |>
    str_to_sentence(data_type) |>
    factor(levels = c("Exponential", "Outbreak", "Ebola case study"))
  ) |>
  DT(!(obs_type %in% "retrospective")) # Drop retrospective estimates

# Save fits with convergence issues
clean_diagnostics |>
  DT(max_rhat > 1.05) |>
  fwrite(
    here("data", "diagnostics", "summary", "convergence_issues.csv")
  )
# Save fits with issues with divergent transitions
clean_diagnostics |>
  DT(per_divergent_transitions > 0.01) |>
  fwrite(
    here("data", "diagnostics", "summary", "divergent_transitions.csv")
  )
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

# Plot mean and distribution of runtimes by model and case study for each
# sample size
runtime_plot <- clean_diagnostics |>
  copy() |>
  DT(, value := run_time) |>
  setkey(sample_size) |>
  DT(,
   mean_run_time := mean(value),
   keyby = c("sample_size", "data_type", "model")
  ) |>
  plot_recovery(y = model, fill = sample_size, alpha = 0.6) +
  geom_point(
    aes(x = mean_run_time, col = sample_size),
    size = 2, shape = 5, alpha = 1,
    position = position_nudge(y = 0)
  ) +
  facet_wrap(
    vars(data_type),
    scales = "free_x"
  ) +
  scale_x_log10() +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  guides(fill = guide_legend(title = "Sample size", nrow = 1),
         col =  guide_none()) +
  labs(
    y = "Model", x = "Run time (minutes)"
  ) +
  theme(legend.position = "bottom")

# Plot Percentage with divergent transitions
divergent_transitions_plot <- clean_diagnostics |>
  DT(per_divergent_transitions > 0.01) |>
  ggplot() +
  aes(
    y = data_type, x = per_divergent_transitions,
    col = distribution, size = sample_size,
    shape = model
  ) +
  geom_point(position = position_jitter(width = 0), alpha = 0.6) +
  scale_x_continuous(labels = scales::percent, trans = "logit") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  labs(
    x = "Divergent transitions",
    y = "Data type"
  ) +
  guides(
    size = guide_legend(title = "Sample size"),
    col = guide_legend(title = "Distribution"),
    ncol = 2
  ) +
  theme(legend.position = "bottom")


exponential_divergent_transitions_plot <- clean_diagnostics |>
  DT(per_divergent_transitions > 0.01) |>
  DT(data_type %in% "Exponential") |>
  ggplot() +
  aes(
    y = scenario,
    x = per_divergent_transitions,
    col = distribution,
    shape = model
  ) +
  geom_point(position = position_jitter(width = 0), alpha = 0.6) +
  scale_x_continuous(labels = scales::percent, trans = "logit") +
  theme_bw() +
  scale_fill_brewer(
    palette = "Dark2", aesthetics = c("colour", "fill")
  ) +
  labs(
    x = "Divergent transitions",
    y = "Growth rate"
  ) +
  guides(
    col = guide_legend(title = "Distribution")
  ) +
  theme(legend.position = "bottom")



outbreak_divergent_transitions_plot <- clean_diagnostics |>
  DT(per_divergent_transitions > 0.01) |>
  DT(data_type %in% "Outbreak") |>
  ggplot() +
  aes(
    y = scenario,
    x = per_divergent_transitions,
    col = distribution,
    size = sample_size,
    shape = model
  ) +
  geom_point(position = position_jitter(width = 0), alpha = 0.6) +
  scale_x_continuous(labels = scales::percent, trans = "logit") +
  theme_bw() +
  scale_fill_brewer(
    palette = "Dark2", aesthetics = c("colour", "fill")
  ) +
  labs(
    x = "Divergent transitions",
    y = "Outbreak observation day"
  ) +
  guides(
    size = guide_legend(title = "Sample size"),
    col = guide_legend(title = "Distribution")
  ) +
  theme(legend.position = "bottom")

  ## Combine plots
  diagnostic_plot <- (runtime_plot + plot_layout(guides = "keep")) /
  (
    divergent_transitions_plot +
    exponential_divergent_transitions_plot +
    outbreak_divergent_transitions_plot +
    plot_layout(guides = "collect")
  ) +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "keep") &
    theme(legend.position = "bottom")

# Save combined plots
ggsave(
  here("figures", "diagnostic.png"),  diagnostic_plot,
  height = 12, width = 16, dpi = 330
)