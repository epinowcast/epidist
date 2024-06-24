set.seed(101)

obs_time <- 25
sample_size <- 500

meanlog <- 1.8
sdlog <- 0.5

# The mean of the lognormal distribution is: exp(meanlog + 0.5 * sdlog^2)

sim_obs <- simulate_gillespie() |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time)

sim_obs <- sim_obs[sample(seq_len(.N), sample_size, replace = FALSE)]

set.seed(101)

shape <- 2
rate <- 3

# The mean of the gamma distribution is: shape * rate

sim_obs_gamma <- simulate_gillespie() |>
  simulate_secondary(
    dist = rgamma,
    shape = shape,
    rate = rate
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time)

sim_obs_gamma <-
  sim_obs_gamma[sample(seq_len(.N), sample_size, replace = FALSE)]
