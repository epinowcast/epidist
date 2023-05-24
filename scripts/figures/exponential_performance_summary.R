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
    fct_rev()) |>
  DT(, pmf := map2(
    meanlog, sdlog,
    ~ dlnorm(seq(0, 20, by = 0.1), meanlog = .x, sdlog = .y)
  ))

clean_e_samples <- e_samples |>
  draws_to_long() |>
  DT(parameter %in% c("mean", "sd")) |>
  make_relative_to_truth(
    draws_to_long(distributions) |>
      setnames("scenario", "distribution"),
    by = c("parameter", "distribution")
  )

exp_summary <- clean_e_samples |>
  group_by(parameter, distribution, scenario, model, replicate) %>%
  summarize(
    median = median(rel_value),
    error = log(median),
    lwr = quantile(rel_value, 0.05, na.rm = TRUE),
    upr = quantile(rel_value, 0.95, na.rm = TRUE)
  ) %>%
  summarize(
    bias = median(error),
    rmse = sqrt(mean(error^2)),
    coverage = mean(lwr < 1 & 1 < upr)
  ) |>
  DT(, model := factor(model, levels = models$model)) |>
  DT(, r := factor(scenario,
    levels = growth_rates$scenario,
    labels = growth_rates$r
  )) |>
  DT(, parameter := stringr::str_to_title(parameter))

g1 <- ggplot(exp_summary) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(bias, r, col = distribution), size = 4, alpha = 0.7) +
  facet_grid(model ~ parameter,
    labeller = label_wrap_gen(multi_line = TRUE, width = 20)
  ) +
  scale_x_continuous("Bias") +
  scale_y_discrete("Growth rate") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(
    panel.spacing = unit(0, "mm"), legend.position = "bottom"
  )

g2 <- ggplot(exp_summary) +
  geom_point(aes(rmse, r, col = distribution), size = 4, alpha = 0.7) +
  facet_grid(model ~ parameter,
    labeller = label_wrap_gen(multi_line = TRUE, width = 20)
  ) +
  scale_x_sqrt("RMSE") +
  scale_y_discrete("Growth rate") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(
    panel.spacing = unit(0, "mm"), legend.position = "bottom"
  )

g3 <- ggplot(exp_summary) +
  annotate(
    "rect", xmin = binom.test(18, 20)[[4]][1],
     xmax = binom.test(18, 20)[[4]][2], ymin = -Inf, ymax = Inf,
     fill = "gray", alpha = 0.5
  ) +
  geom_vline(xintercept = 0.9, lty = 2) +
  geom_point(aes(coverage, r, col = distribution), size = 4, alpha = 0.7) +
  facet_grid(model ~ parameter,
    labeller = label_wrap_gen(multi_line = TRUE, width = 20)
  ) +
  scale_x_continuous("Coverage") +
  scale_y_discrete("Growth rate") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(
    panel.spacing = unit(0, "mm"), legend.position = "bottom"
  )

exponential_performance_summary <- (
    g2 + theme(
      strip.text.y = element_blank(), strip.background.y = element_blank()
    )
  ) +
  (
    g1 + theme(
      strip.text.y = element_blank(), strip.background.y = element_blank(),
      axis.text.y = element_blank(), axis.title.y = element_blank()
    )
  ) +
  (
    g3 + theme(
      axis.text.y = element_blank(), axis.title.y = element_blank()
      )
  ) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", width = c(1, 1, 1)) &
  theme(legend.position = "bottom") &
  guides(col = guide_legend(title = "Distribution"))

# Save combined plots
ggsave(
  here("figures", "exponential_performance_summary.pdf"),
  exponential_performance_summary,
  height = 20, width = 20, dpi = 330
)
