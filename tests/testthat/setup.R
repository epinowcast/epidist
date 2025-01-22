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
  dplyr::mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_lwr + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_lwr + 1,
    obs_time = obs_time
  ) |>
  dplyr::filter(.data$stime_upr <= .data$obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs <- as_epidist_linelist_data(
  sim_obs$ptime_lwr,
  sim_obs$ptime_upr,
  sim_obs$stime_lwr,
  sim_obs$stime_upr,
  sim_obs$obs_time
)

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
  dplyr::mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_lwr + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_lwr + 1,
    obs_time = obs_time
  ) |>
  dplyr::filter(.data$stime_upr <= .data$obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs_gamma <- as_epidist_linelist_data(
  sim_obs_gamma$ptime_lwr,
  sim_obs_gamma$ptime_upr,
  sim_obs_gamma$stime_lwr,
  sim_obs_gamma$stime_upr,
  sim_obs_gamma$obs_time
)

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
  dplyr::mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_lwr + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_lwr + 1,
    obs_time = obs_time
  ) |>
  dplyr::filter(.data$stime_upr <= .data$obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Temporary solution for classing time data
sim_obs_sex <- as_epidist_linelist_data(
  sim_obs_sex$ptime_lwr,
  sim_obs_sex$ptime_upr,
  sim_obs_sex$stime_lwr,
  sim_obs_sex$stime_upr,
  sim_obs_sex$obs_time,
  sex = sim_obs_sex$sex
)
prep_obs <- as_epidist_latent_model(sim_obs)
prep_naive_obs <- as_epidist_naive_model(sim_obs)
prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
prep_obs_gamma <- as_epidist_latent_model(sim_obs_gamma)
prep_obs_sex <- as_epidist_latent_model(sim_obs_sex)

prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
prep_marginal_obs_gamma <- as_epidist_marginal_model(sim_obs_gamma)
prep_marginal_obs_sex <- as_epidist_marginal_model(sim_obs_sex)

if (not_on_cran()) {
  set.seed(1)
  cli::cli_inform("Compiling the latent model with cmdstanr")
  fit <- epidist(
    data = prep_obs, seed = 1, chains = 2, cores = 2, silent = 2, refresh = 0,
    backend = "cmdstanr"
  )

  cli::cli_inform("Compiling the latent model with rstan")
  fit_rstan <- epidist(
    data = prep_obs, seed = 1, chains = 2, cores = 2, silent = 2, refresh = 0
  )

  cli::cli_inform("Compiling the marginal model with cmdstanr")
  fit_marginal <- suppressMessages(epidist(
    data = prep_marginal_obs, seed = 1, chains = 2, cores = 2, silent = 2,
    refresh = 0, backend = "cmdstanr"
  ))

  cli::cli_inform("Compiling the latent model with cmdstanr and a gamma dist")
  fit_gamma <- epidist(
    data = prep_obs_gamma, family = Gamma(link = "log"),
    seed = 1, chains = 2, cores = 2, silent = 2, refresh = 0,
    backend = "cmdstanr"
  )

  cli::cli_inform("Compiling the marginal model with cmdstanr and a gamma dist")
  fit_marginal_gamma <- suppressMessages(epidist(
    data = prep_marginal_obs_gamma, family = Gamma(link = "log"),
    seed = 1, chains = 2, cores = 2, silent = 2, refresh = 0,
    backend = "cmdstanr"
  ))

  cli::cli_inform(
    "Compiling the latent model with cmdstanr and a sex stratification"
  )
  fit_sex <- epidist(
    data = prep_obs_sex,
    formula = bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    seed = 1, silent = 2, refresh = 0,
    cores = 2, chains = 2, backend = "cmdstanr"
  )

  cli::cli_inform(
    "Compiling the marginal model with cmdstanr and a sex stratification"
  )
  fit_marginal_sex <- suppressMessages(epidist(
    data = prep_marginal_obs_sex,
    formula = bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    seed = 1, silent = 2, refresh = 0,
    cores = 2, chains = 2, backend = "cmdstanr"
  ))
}
