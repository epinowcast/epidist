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
  filter_obs_by_obs_time(obs_time = 30) |>
  DT(sample(1:.N, 400, replace = FALSE))

latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 100
)

standata <- list(
  N=nrow(truncated_obs),
  ptime_lwr=truncated_obs$ptime_lwr,
  ptime_upr=truncated_obs$ptime_upr,
  stime_lwr=truncated_obs$stime_lwr,
  stime_upr=truncated_obs$stime_upr,
  end_t=30
)

model <- cmdstan_model("scripts/lognormal_doublecensor.stan")

myfit <- model$sample(data=standata, cores=4)

myfit$time()

rstan::get_elapsed_time(latent_truncation_censoring_fit$fit) 
