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
  DT(sample(1:.N, 200, replace = FALSE))

standata <- list(
  r=0.2,
  max_delay=40,
  N=nrow(truncated_obs),
  Y=truncated_obs$delay_daily
)

model <- cmdstan_model("scripts/lognormal_dynamical.stan")

myfit <- model$sample(data=standata, chains = 1)

myfit

msumm <- summary(myfit)

plot(msumm$summary[201:400,1], msumm$summary[1:200,1])

msumm$summary[401:402,]
