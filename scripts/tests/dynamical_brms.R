library(dynamicaltruncation)
library(data.table)
library(cmdstanr)

outbreak <- simulate_exponential_cases(sample_size = 10000, seed=101)

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

truncated_obs$r <- 0.2
truncated_obs$max_delay <- 20

myfit <- exponential_delay(data=truncated_obs, cores = 4)

myfit
