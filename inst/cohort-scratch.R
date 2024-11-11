library(dplyr)
library(ggplot2)
library(brms)
library(primarycensored)

set.seed(101)

obs_time <- 25
sample_size <- 500

meanlog <- 1.8
sdlog <- 0.5

sim_obs <- simulate_gillespie() |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Create cohort version of data

cohort_obs <- sim_obs |>
  group_by(delay = delay_daily) |>
  summarise(n = n())

ggplot(cohort_obs, aes(x = delay, y = n)) +
  geom_col()
