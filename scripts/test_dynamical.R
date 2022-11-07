library(dynamicaltruncation)
library(data.table)
library(cmdstanr)

growth_rate <- 0.2

outbreak <- simulate_exponential_cases(
  r = growth_rate, sample_size = 10000, seed=101
)

secondary_dist <- data.table(
  meanlog = 1.8, sdlog = 0.5
) |>
  add_natural_scale_mean_sd()

obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$meanlog[[1]],
    sdlog = secondary_dist$sdlog[[1]]
  ) |>
  observe_process()

set.seed(101)
truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 25) |>
  DT(sample(1:.N, 200, replace = FALSE))

standata <- list(
  r = growth_rate,
  max_delay = 20,
  N = nrow(truncated_obs),
  Y = truncated_obs$delay_daily
)

model <- cmdstan_model("scripts/lognormal_dynamical.stan")

myfit <- model$sample(data = standata, chains = 2, parallel_chains = 2)

myfit
