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
  DT(sample(1:.N, 200, replace = FALSE))

standata <- list(
  N=nrow(truncated_obs),
  ptime_lwr=truncated_obs$ptime_lwr,
  ptime_upr=truncated_obs$ptime_upr,
  stime_lwr=truncated_obs$stime_lwr,
  stime_upr=truncated_obs$stime_upr,
  end_t=46
)

model <- stan_model("scripts/lognormal_doublecensor_reparam.stan")

myfit <- sampling(model,standata,iter=2000,chains=4)

msumm <- summary(myfit)

plot(msumm$summary[201:400,1], msumm$summary[1:200,1])

msumm$summary[401:402,]
