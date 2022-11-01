library(data.table)
library(purrr, quietly = TRUE)
library(dplyr)
library(here)
library(rstan)
options(mc.cores=4)

functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)


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
  DT(sample(1:.N, 200, replace = FALSE))

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

model <- stan_model("scripts/lognormal_doublecensor.stan")

myfit <- sampling(model,standata,iter=2000,chains=4)

lsumm <- summary(latent_truncation_censoring_fit)
msumm <- summary(myfit)

lsumm
msumm$summary[401:402,]

plot(msumm$summary[201:400,1], msumm$summary[1:200,1])
