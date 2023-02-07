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

# Load simulated cases
exponential_obs <- fread(here("data/scenarios/exponential-simulation.csv"))

# Load available models
models <- fread(here("data/meta/models.csv")) |>
  DT(, model := factor(model))

# Function to read each arrow dataset, filter for the simulation
# and attach the model name
read_exponential <- function(target_model) {
  # Load posterior samples as an arrow dataset
  dataset <- open_dataset(
    here("data", "posteriors", models[model %in% target_model, in_code])
  )

  # Filter for the case study related samples
  # Add model name to each row
  samples <- dataset |>
    filter(data_type %in% "exponential") |>
    mutate(model = target_model) |>
    collect()

  return(samples)
}

# Load exponential samples for each model and combine
e_samples <- map_dfr(models$model, read_exponential)

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
  ) |>
  DT(, pmf := map2(
    meanlog, sdlog,
    ~ dlnorm(seq(0, 20, by = 0.1), meanlog = .x, sdlog = .y)
    )
  )

# Make inidividual plots

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_exponential_obs <- filter_obs_by_obs_time(
  exponential_obs, obs_time = 30
)

empirical_pmf_plot <- truncated_exponential_obs |>
  DT(delay_daily <= 20) |>
  DT(
    distributions[, .(distribution = scenario, distribution_stat)],
    on = "distribution"
  ) |>
  DT(, r := factor(r)) |>
  ggplot() +
  aes(x = delay_daily) +
  geom_histogram(
  aes(y = after_stat(density), fill = r),
    binwidth = 1, position = "dodge",
    col = "#696767b1"
  ) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Days", y = "Density") +
  guides(fill = guide_legend(title = "Growth rate", reverse = FALSE)) +
  geom_line(
    data = distributions |>
      DT(,
        .(pmf = unlist(pmf), delay_daily = seq(0, 20, by = 0.1)),
        by = "distribution_stat"
      ),
    aes(y = pmf, fill = NULL), col = "black"
) +
facet_wrap(vars(distribution_stat), ncol = 1)

# Summarise draws

# TODO: Due to the number of samples this plot is very slow to make. Can we speed it up? # nolint

# TODO: The non-latent truncation and censoring model currently has a very odd looking posterior. Does this indicate a bug or just some kind of bias in the model. # nolint

# TODO: This currently roles all the replicates into one plot. We might want another version (for the SI) that splits these out so we can explore the amount of variation. # nolint
# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- e_samples |>
  DT(,
    c(
      "sample_size", "data_type", "id", "obs_type"
    ) := NULL
  ) |>
  draws_to_long() |>
  DT(parameter %in% c("mean", "sd")) |>
  make_relative_to_truth(
    draws_to_long(distributions)[parameter %in% c("mean", "sd")] |>
      DT(, pmf := NULL) |>
      setnames("scenario", "distribution"),
    by = c("parameter", "distribution")
  ) |>
  DT(, distribution_stat :=  distribution_stat |>
        str_to_sentence() |>
        factor() |>
        fct_rev()
  ) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  DT(, model := factor(model, levels = models$model)) |>
  DT(growth_rates, on = "scenario") |>
  DT(, r := factor(r)) |>
  DT(rel_value <= 2) |>
  DT(rel_value >= 0.1) |>
  plot_relative_recovery(y = r, fill = distribution_stat) +
  facet_grid(
    vars(model), vars(parameter),
    labeller = label_wrap_gen(multi_line = TRUE),
    scales = "free_x"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(title = "Distribution"), col = guide_none()) +
  labs(
    y = "Growth rate", x = "Relative to ground truth"
  ) +
  theme(legend.position = "bottom")


# Combine plots
exponential_plot <- empirical_pmf_plot + parameter_density_plot +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", width = c(1, 2)) &
  theme(legend.position = "bottom")

# Save combined plots
ggsave(
  here("figures", "exponential.png"), exponential_plot,
  height = 16, width = 16, dpi = 330
)