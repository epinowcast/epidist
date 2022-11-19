library(dynamicaltruncation)
library(data.table)
library(cmdstanr)

outbreak <- simulate_exponential_cases(sample_size = 10000, seed=101)

## super short delays
secondary_dist <- data.table(
  meanlog = 0.3, sdlog = 0.3
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

myfit <- latent_truncation_censoring_adjusted_delay_zero(data=truncated_obs, cores = 4)

myfit
