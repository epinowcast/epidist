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

agg_sim_obs <- as_epidist_aggregate_data(sim_obs)

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

# Simulate from a Weibull distribution

shape_weibull <- 2
scale_weibull <- 7

sim_obs_weibull <- simulate_gillespie() |>
  simulate_secondary(
    dist = rweibull,
    shape = shape_weibull,
    scale = scale_weibull
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
sim_obs_weibull <- as_epidist_linelist_data(
  sim_obs_weibull$ptime_lwr,
  sim_obs_weibull$ptime_upr,
  sim_obs_weibull$stime_lwr,
  sim_obs_weibull$stime_upr,
  sim_obs_weibull$obs_time
)

agg_sim_obs_weibull <- as_epidist_aggregate_data(sim_obs_weibull)

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

agg_sim_obs_sex <- as_epidist_aggregate_data(sim_obs_sex, by = "sex")

# Weekly censoring windows against the default delay, which leaves a third or
# so of observations with overlapping primary and secondary windows. This is
# the case the latent model Jacobian adjustment applies to.
set.seed(101)

overlap_n <- sample_size

sim_obs_overlap <- simulate_gillespie(seed = 101) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  dplyr::mutate(
    ptime_lwr = 7 * floor(.data$ptime / 7),
    ptime_upr = .data$ptime_lwr + 7,
    stime_lwr = 7 * floor(.data$stime / 7),
    stime_upr = .data$stime_lwr + 7,
    obs_time = 35
  ) |>
  dplyr::filter(.data$stime_upr <= .data$obs_time) |>
  dplyr::slice_sample(n = overlap_n, replace = FALSE)

sim_obs_overlap <- as_epidist_linelist_data(
  sim_obs_overlap$ptime_lwr,
  sim_obs_overlap$ptime_upr,
  sim_obs_overlap$stime_lwr,
  sim_obs_overlap$stime_upr,
  sim_obs_overlap$obs_time
)

prep_obs_overlap <- as_epidist_latent_model(sim_obs_overlap)
prep_marginal_obs_overlap <- suppressMessages(
  as_epidist_marginal_model(sim_obs_overlap)
)

prep_obs <- as_epidist_latent_model(sim_obs)
prep_naive_obs <- as_epidist_naive_model(sim_obs)
prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
prep_obs_gamma <- as_epidist_latent_model(sim_obs_gamma)
prep_obs_sex <- as_epidist_latent_model(sim_obs_sex)

prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
prep_marginal_obs_gamma <- as_epidist_marginal_model(sim_obs_gamma)
prep_marginal_obs_sex <- as_epidist_marginal_model(sim_obs_sex)
prep_marginal_obs_weibull <- as_epidist_marginal_model(sim_obs_weibull)

# Published summary estimates for the meta model. Study A reports naive
# daily discretised summaries from a right truncated snapshot, study B a
# fully adjusted mean and quantile, and study C a quantile from a study
# that only corrected the secondary interval.
sim_estimates <- suppressMessages(as_epidist_estimates_data(
  data.frame(
    study = c("A", "A", "B", "B", "C"),
    type = c("mean", "sd", "mean", "quantile", "quantile"),
    value = c(7.5, 3.6, 6.4, 11.2, 5.4),
    p = c(NA, NA, NA, 0.9, 0.5),
    n = c(120, 120, 80, 80, 200),
    relative_obs_time = c(20, 20, Inf, Inf, 30),
    trunc_adjusted = c(FALSE, FALSE, TRUE, TRUE, FALSE),
    cens_adjusted = c(0, 0, 1, 1, 2),
    stringsAsFactors = FALSE
  )
))

prep_meta_individual <- suppressMessages(as_epidist_meta_model(sim_obs))
prep_meta_estimates <- suppressMessages(
  as_epidist_meta_model(estimates = sim_estimates)
)
prep_meta_obs <- suppressMessages(
  as_epidist_meta_model(sim_obs, estimates = sim_estimates)
)

# The shared fits below use the cmdstanr backend, so they are only built
# when cmdstanr and CmdStan are both available. Tests that use them call
# `skip_if_no_cmdstanr()`.
if (not_on_cran() && has_cmdstanr()) {
  set.seed(1)
  cli::cli_alert_info("Compiling the latent model with cmdstanr")
  fit <- epidist(
    data = prep_obs,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  )

  cli::cli_alert_info("Compiling the latent model with rstan")
  fit_rstan <- epidist(
    data = prep_obs,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000
  )

  cli::cli_alert_info("Compiling the marginal model with cmdstanr")
  fit_marginal <- suppressMessages(epidist(
    data = prep_marginal_obs,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  ))

  cli::cli_alert_info("Compiling the naive model with cmdstanr")
  fit_naive <- epidist(
    data = prep_naive_obs,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  )

  cli::cli_alert_info(
    "Compiling the latent model with cmdstanr and a gamma dist"
  )
  fit_gamma <- epidist(
    data = prep_obs_gamma,
    family = Gamma(link = "log"),
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  )

  cli::cli_alert_info(
    "Compiling the marginal model with cmdstanr and a weibull dist"
  )
  fit_marginal_weibull <- epidist(
    data = prep_marginal_obs_weibull,
    family = "weibull",
    seed = 1,
    chains = 2,
    cores = 2,
    iter = 1000,
    backend = "cmdstanr"
  )

  cli::cli_alert_info(
    "Compiling the marginal model with cmdstanr and a gamma dist"
  )
  fit_marginal_gamma <- suppressMessages(epidist(
    data = prep_marginal_obs_gamma,
    family = Gamma(link = "log"),
    seed = 1,
    chains = 2,
    cores = 2,
    iter = 1000,
    backend = "cmdstanr"
  ))

  cli::cli_alert_info(
    "Compiling the latent model with cmdstanr and a sex stratification"
  )
  fit_sex <- epidist(
    data = prep_obs_sex,
    formula = bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    seed = 1,
    iter = 1000,
    cores = 2,
    chains = 2,
    backend = "cmdstanr"
  )

  cli::cli_alert_info("Compiling the latent model with overlapping windows")
  fit_overlap <- epidist(
    data = prep_obs_overlap,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    backend = "cmdstanr"
  )

  cli::cli_alert_info("Compiling the marginal model with overlapping windows")
  fit_marginal_overlap <- suppressMessages(epidist(
    data = prep_marginal_obs_overlap,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    backend = "cmdstanr"
  ))

  cli::cli_alert_info(
    "Compiling the marginal model with cmdstanr and a sex stratification"
  )
  fit_marginal_sex <- suppressMessages(epidist(
    data = prep_marginal_obs_sex,
    formula = bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    seed = 1,
    iter = 1000,
    cores = 2,
    chains = 2,
    backend = "cmdstanr"
  ))

  # Synthetic "published" estimates produced by applying naive estimators
  # (integer date differences, no right truncation adjustment) to samples
  # from the same lognormal delay distribution used above.
  # A study observing up to study_obs_time keeps an integer delay k when
  # k + 1 <= study_obs_time, which is the same conditioning the meta model
  # applies through its grid.
  set.seed(2)
  naive_summaries <- function(size, study_obs_time) {
    ptime <- stats::runif(size, 0, 1)
    delay <- stats::rlnorm(size, meanlog, sdlog)
    obs <- floor(ptime + delay)
    obs <- obs[obs + 1 <= study_obs_time]
    return(list(
      mean = mean(obs), sd = stats::sd(obs), size = length(obs),
      q90 = stats::quantile(obs, 0.9, names = FALSE)
    ))
  }

  study_obs_times <- c(12, 15, 18, 20, 25, 30)
  naive_draws <- lapply(study_obs_times, naive_summaries, size = 2000)

  sim_biased_estimates <- suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = rep(paste0("study_", seq_along(study_obs_times)), each = 3),
      type = rep(c("mean", "sd", "quantile"), times = length(study_obs_times)),
      value = as.numeric(rbind(
        vapply(naive_draws, `[[`, numeric(1), "mean"),
        vapply(naive_draws, `[[`, numeric(1), "sd"),
        vapply(naive_draws, `[[`, numeric(1), "q90")
      )),
      p = rep(c(NA, NA, 0.9), times = length(study_obs_times)),
      n = rep(vapply(naive_draws, `[[`, numeric(1), "size"), each = 3),
      relative_obs_time = rep(study_obs_times, each = 3),
      trunc_adjusted = FALSE,
      cens_adjusted = 0,
      stringsAsFactors = FALSE
    )
  ))

  prep_meta_biased <- suppressMessages(
    as_epidist_meta_model(estimates = sim_biased_estimates)
  )
  prep_meta_mixed <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    estimates = sim_biased_estimates
  ))

  cli::cli_alert_info(
    "Compiling the meta model with cmdstanr and summary estimates only"
  )
  fit_meta_estimates <- suppressMessages(epidist(
    data = prep_meta_biased,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  ))

  cli::cli_alert_info(
    "Compiling the meta model with cmdstanr and mixed data"
  )
  fit_meta_mixed <- suppressMessages(epidist(
    data = prep_meta_mixed,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000,
    backend = "cmdstanr"
  ))
}
