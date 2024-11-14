set.seed(101)

as_epidist_linelist_time <- function(data) {
  class(data) <- c("epidist_linelist", class(data))
  epidist_validate_data(data)
  return(data)
}

filter_obs_by_obs_time <- function(linelist, obs_time) {
  linelist |>
    mutate(
      obs_time = obs_time,
      relative_obs_time = .data$obs_time - .data$ptime,
    ) |>
    filter(.data$stime_upr <= .data$obs_time)
}

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
  filter_obs_by_obs_time(obs_time = obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs <- as_epidist_linelist_time(sim_obs)

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
  filter_obs_by_obs_time(obs_time = obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs_gamma <- as_epidist_linelist_time(sim_obs_gamma)

# Data with a sex difference

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
  ) |>
  dplyr::select(case, ptime, delay, stime, sex)

sim_obs_sex_f <- dplyr::filter(sim_obs_sex, sex == 1) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog_f,
    sdlog = sdlog_f
  ) |>
  dplyr::select(case, ptime, delay, stime, sex)

sim_obs_sex <- dplyr::bind_rows(sim_obs_sex_m, sim_obs_sex_f) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs_sex <- as_epidist_linelist_time(sim_obs_sex)
