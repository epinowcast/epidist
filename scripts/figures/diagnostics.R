# Load packages
# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation
library(forcats) # manipulate factors

# Load available models
models <- fread(here("data/meta/models.csv")) |>
  rbind(data.table(
    model = "Joint incidence and delay estimation",
    in_code = "epinowcast"
  )) |>
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

# Load growth rates
growth_rates <- fread(here("data/meta/growth_rates.csv")) |>
  DT(, growth_rate := paste0(
    str_to_sentence(scenario),
    " (", round(r, 1), ")"
  )) |>
  DT(, growth_rate := factor(growth_rate))

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
  DT(!(obs_type %in% "retrospective")) |> # Drop retrospective estimates
  merge(outbreak_estimation_times, by = "scenario", all = TRUE) |>
  merge(distributions,  by.x = "distribution", by.y = "scenario", all = TRUE) |>
  merge(growth_rates, by = "scenario", all = TRUE) |>
  DT(, distribution_stat :=  distribution_stat |>
        str_to_sentence() |>
        factor() |>
        fct_rev()
  ) |>
  DT(, r := factor(r))

# Save fits with convergence issues
convergence_issues <- clean_diagnostics |>
  DT(max_rhat > 1.05)

if (nrow(convergence_issues) > 0) {
  message(
    "There were ", nrow(convergence_issues), " convergence issues"
  )
}

fwrite(
  convergence_issues,
  here("data", "diagnostics", "summary", "convergence_issues.csv")
)

# Save fits with issues with divergent transitions
divergent_transitions <- clean_diagnostics |>
  DT(per_divergent_transitions > 0.01)

if (nrow(divergent_transitions) > 0) {
  message(
    "There were ", nrow(divergent_transitions), " divergent transitions"
  )
}

fwrite(
  divergent_transitions,
  here("data", "diagnostics", "summary", "divergent_transitions.csv")
)

# Save any failed fits
if (any(colnames(clean_diagnostics) %in% "error")) {
  failed_fits <- clean_diagnostics |>
    DT(!is.na(error)) |>
    DT(!error %in% "")

  message(
    "There were ", nrow(failed_fits), " failed fits"
  )
  fwrite(
    failed_fits, here("data", "diagnostics", "summary", "failed_fits.csv")
  )
}

data.table(
  `Failed fits` = nrow(failed_fits),
  `Convergence issues` = nrow(convergence_issues),
  `Divergent transitions` = nrow(divergent_transitions)
) |>
  fwrite(
    here("data", "diagnostics", "summary", "summary.csv")
  )

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
    size = 4, shape = 5, alpha = 1,
    position = position_nudge(y = 0)
  ) +
  facet_wrap(
    vars(data_type),
    scales = "free_x"
  ) +
  scale_x_log10() +
  scale_y_discrete(labels = (\(x) str_wrap(x, width = 20))) +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  guides(fill = guide_legend(title = "Sample size", nrow = 1),
         col =  guide_none()) +
  labs(
    y = "Model", x = "Run time (minutes)"
  ) +
  theme(legend.position = "bottom")

# Plot Percentage with divergent transitions
# Filter for >0.1% divergent transitions
divergent_transitions_plot <- clean_diagnostics |>
  DT(per_divergent_transitions > 0.001) |>
  ggplot() +
  aes(
    y = model, x = per_divergent_transitions,
    col = distribution_stat, size = sample_size
  ) +
  geom_point(position = position_jitter(width = 0), alpha = 0.6) +
  scale_x_continuous(labels = scales::percent, trans = "logit") +
  theme_bw() +
  scale_y_discrete(labels = (\(x) str_wrap(x, width = 20))) +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  labs(
    x = "Divergent transitions",
    y = "Data type"
  ) +
  guides(
    size = guide_legend(title = "Sample size", nrow = 2),
    col = guide_legend(title = "Distribution", nrow = 2)
  ) +
  theme(legend.position = "bottom") +
  facet_grid(, vars(data_type))

# if (
#   clean_diagnostics |>
#     DT(per_divergent_transitions > 0.01) |>
#     DT(data_type %in% "Exponential") |>
#     DT(, unique(model)) |>
#     (\(x) x != "Truncation and censoring adjusted") ()
#   ) {
#     stop("Exponential data type should only have one model")
# }

# exponential_divergent_transitions_plot <- clean_diagnostics |>
#   DT(per_divergent_transitions > 0.01) |>
#   DT(data_type %in% "Exponential") |>
#   ggplot() +
#   aes(
#     y = r,
#     x = per_divergent_transitions,
#     col = distribution_stat,
#     shape = model
#   ) +
#   geom_point(position = position_jitter(width = 0), alpha = 0.6) +
#   scale_x_continuous(labels = scales::percent, trans = "logit") +
#   theme_bw() +
#   scale_fill_brewer(
#     palette = "Dark2", aesthetics = c("colour", "fill")
#   ) +
#   labs(
#     x = "Divergent transitions",
#     y = "Growth rate"
#   ) +
#   guides(
#     col = guide_legend(title = "Distribution", nrow = 2),
#     shape = guide_none()
#   ) +
#   theme(legend.position = "bottom")



# outbreak_divergent_transitions_plot <- clean_diagnostics |>
#   DT(per_divergent_transitions > 0.001) |>
#   DT(data_type %in% "Outbreak") |>
#   DT(, time  := time |> factor() |> fct_rev()) |>
#   ggplot() +
#   aes(
#     y = time,
#     x = per_divergent_transitions,
#     col = distribution_stat,
#     size = sample_size,
#     shape = model
#   ) +
#   geom_point(position = position_jitter(width = 0), alpha = 0.6) +
#   scale_x_continuous(labels = scales::percent, trans = "logit") +
#   theme_bw() +
#   scale_fill_brewer(
#     palette = "Dark2", aesthetics = c("colour", "fill")
#   ) +
#   labs(
#     x = "Divergent transitions",
#     y = "Outbreak observation day"
#   ) +
#   guides(
#     size = guide_legend(title = "Sample size", nrow = 2),
#     col = guide_legend(title = "Distribution", nrow = 2),
#     shape = guide_legend(title = "Model", nrow = 2)
#   ) +
#   theme(legend.position = "bottom")

  ## Combine plots
  diagnostic_plot <- runtime_plot +
    divergent_transitions_plot +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "keep", heights = c(3, 1)) &
    theme(legend.position = "bottom", legend.direction = "vertical")

# Save combined plots
ggsave(
  here("figures", "diagnostic.pdf"),  diagnostic_plot,
  height = 12, width = 12, dpi = 330
)