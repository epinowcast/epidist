devtools::load_all() # for loading the local package functions
library(dynamicaltruncation) ## for refitting
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
## not sure if I need all of them but just taking them...

# Load simulated cases
exponential_obs <- fread(here("data/scenarios/exponential-simulation.csv"))

truncated_exponential_obs <- filter_obs_by_obs_time(
  exponential_obs,
  obs_time = 30
)

# Load available models
models <- fread(here("data/meta/models.csv")) |>
  DT(, model := factor(model))

# Load growth rates
growth_rates <- fread(here("data/meta/growth_rates.csv")) |>
  DT(, growth_rate := paste0(
    str_to_sentence(scenario),
    " (", round(r, 1), ")"
  )) |>
  DT(, growth_rate := factor(growth_rate))

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
e_samples <- map_dfr(
  "Latent variable truncation and censoring adjusted", read_exponential
)

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

e_samples_filter <- e_samples %>%
  filter(distribution == "long") %>%
  filter(scenario %in% c("fast growth", "fast decay", "stable")) |>
  DT(growth_rates, on = "scenario") |>
  DT(, r := factor(r))

# Make a clean samples data.frame for plotting
clean_e_samples <- e_samples_filter |>
  copy() |>
  DT(
    ,
    c(
      "sample_size", "data_type", "id", "obs_type"
    ) := NULL
  ) |>
  draws_to_long() |>
  DT(parameter %in% c("mean", "sd")) |>
  make_relative_to_truth(
    draws_to_long(distributions) |>
      setnames("scenario", "distribution"),
    by = c("parameter", "distribution")
  ) |>
  DT(, distribution_stat := distribution_stat |>
    str_to_sentence() |>
    factor() |>
    fct_rev()) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  DT(, model := factor(model, levels = models$model)) |>
  DT(,
    sample := 1:.N,
    keyby = c(
      "model", "r", "parameter", "distribution_stat",
      "replicate"
    )
  )

scores <- clean_e_samples |>
  DT(, true_value := 0) |>
  DT(, prediction := log(rel_value)) |>
  DT(
    ,
    c(
      "model", "r", "parameter", "distribution_stat", "replicate",
      "sample", "prediction", "true_value"
    )
  ) |>
  score()

mean_scores <- scores %>%
  group_by(parameter, r) %>%
  summarize(
    mean = mean(bias)
  )

clean_e_samples_summ <- clean_e_samples %>%
  group_by(parameter, replicate, r) %>%
  summarize(
    median = median(value),
    lwr = quantile(value, 0.025),
    upr = quantile(value, 0.975)
  )

distributions_long <- distributions %>%
  filter(scenario == "short") %>%
  select(mean, sd) %>%
  melt() %>%
  rename(
    parameter = variable
  ) %>%
  mutate(
    parameter = factor(parameter,
      levels = c("mean", "sd"),
      labels = c("Mean", "Sd")
    )
  )

truncated_exponential_obs_filter <- truncated_exponential_obs %>%
  filter(
    distribution == "long",
    scenario %in% c("fast decay", "fast growth", "stable")
  )

truncated_exponential_obs_censor <- truncated_exponential_obs_filter %>%
  calculate_censor_delay(additional_by = "r")

dd <- truncated_exponential_obs %>%
  filter(distribution == "long", r == "0") |>
  DT(sample(1:400, replace = FALSE))

fit <- latent_truncation_censoring_adjusted_delay(data = dd)

ss <- posterior_summary(fit)

ctime_est <- data.frame(
  id = 1:200,
  stime = ss[5:204, 1],
  ptime = ss[205:404, 1]
)

g1 <- ggplot(filter(truncated_exponential_obs_censor, type == "ptime")) +
  geom_point(aes(cohort, mean)) +
  geom_errorbar(aes(cohort, ymin = lwr, ymax = upr), width = 0) +
  scale_x_continuous("Daily primary event time") +
  scale_y_continuous("Mean daily censoring") +
  facet_wrap(~r, nrow = 3) +
  ggtitle("A") +
  theme_bw()

g2 <- ggplot(filter(truncated_exponential_obs_censor, type == "stime")) +
  geom_point(aes(cohort, mean)) +
  geom_errorbar(aes(cohort, ymin = lwr, ymax = upr), width = 0) +
  scale_x_continuous("Daily secondary event time") +
  scale_y_continuous("Mean daily censoring") +
  facet_wrap(~r, nrow = 3) +
  ggtitle("B") +
  theme_bw()

g3 <- ggplot(scores) +
  geom_density(aes(bias)) +
  geom_vline(xintercept = 0, lty = 1) +
  geom_vline(data = mean_scores, aes(xintercept = mean), lty = 2) +
  scale_x_continuous("Bias") +
  scale_y_continuous("Density") +
  facet_grid(r ~ parameter) +
  ggtitle("C") +
  theme_bw()

g4 <- ggplot(ctime_est) +
  geom_point(aes(x = ptime, y = stime), size = 0.5) +
  scale_x_continuous("Latent primary event time") +
  scale_y_continuous("Latent secondary event time") +
  ggtitle("D") +
  theme_bw()

gfinal <- g1 + g2 + g3 + g4 +
  plot_layout(height = c(2, 1))

# Save combined plots
ggsave(
  here("figures", "censor.png"), gfinal,
  height = 10, width = 8, dpi = 330
)
