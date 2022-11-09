library(dynamicaltruncation)
library(epinowcast)
library(data.table)
library(cmdstanr)

growth_rate <- 0.2

outbreak <- simulate_exponential_cases(
  r = growth_rate, sample_size = 10000, seed=101
)

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
  filter_obs_by_obs_time(obs_time = 25) |>
  DT(sample(1:.N, 200, replace = FALSE))

dt <- truncated_obs |>
  DT(, reference_date := as.Date("2022-01-01") + ptime_daily) |>
  DT(, report_date := as.Date("2022-01-01") + stime_daily) |>
  DT(, .(reference_date, report_date)) |>
  DT(, .(confirm = .N), by = c("reference_date", "report_date")) |>
  DT(order(reference_date, report_date)) |>
  DT(, confirm := cumsum(confirm), by = "reference_date")

dt <- enw_complete_dates(dt)

pdt <- enw_preprocess_data(dt, max_delay = max(truncated_obs$delay_daily))

fit <- epinowcast(
  data = pdt, 
  reference = enw_reference(~ 1, distribution = "lognormal", data = pdt),
  obs = enw_obs(family = "poisson", data = pdt),
  fit = enw_fit_opts(
    parallel_chains = 2, chains = 2, show_messages = FALSE, adapt_delta = 0.9
  )
)

plot(fit)

extract_epinowcast_lognormal_draws <- function(fit, id_vars) {
  draws <- fit |>
    (\(x) x$fit[[1]]$draws(variables = c("refp_mean", "refp_sd")))() |>
    posterior::as_draws_df() |>
    data.table::as.data.table()

  data.table::setnames(
    draws, c("refp_mean[1]", "refp_sd[1]"), c("meanlog", "sdlog"),
    skip_absent = TRUE
  )
  draws <- draws[, .(meanlog, sdlog)]
  draws <- add_natural_scale_mean_sd(draws)

  if (!missing(id_vars)) {
    draws <- merge(
      draws[, id := id_vars$id], id_vars, by = "id"
    )
  }
  return(draws[])
}

fit |>
  extract_epinowcast_lognormal_draws() |>
  draws_to_long() |>
  summarise_lognormal_draws(sf = 3)
