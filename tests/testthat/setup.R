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
rate <- 1 / 3

# The mean of the gamma distribution is: shape / rate
mu <- shape / rate

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

# What follows is data with a sex difference

meanlog_m <- 2.0
sdlog_m <- 0.3

meanlog_f <- 1.3
sdlog_f <- 0.7

sim_obs_sex <- simulate_gillespie()
sim_obs_sex$sex <- rbinom(n = nrow(sim_obs_sex), size = 1, prob = 0.5)

sim_obs_sex_m <- dplyr::filter(sim_obs_sex, sex == 0) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog_m,
    sdlog = sdlog_m
  )

sim_obs_sex_f <- dplyr::filter(sim_obs_sex, sex == 1) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog_f,
    sdlog = sdlog_f
  )

sim_obs_sex <- bind_rows(sim_obs_sex_m, sim_obs_sex_f) |>
  arrange(case)

sim_obs_sex <- sim_obs_sex |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time)

sim_obs_sex <- sim_obs_sex[sample(seq_len(.N), sample_size, replace = FALSE)]
