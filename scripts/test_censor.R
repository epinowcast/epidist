library(dynamicaltruncation)
library(data.table)
library(purrr)
library(ggplot2)

tfun <- function(r = 0.2, t = 1) {
  (exp(r * t) * (r * t - 1) + 1) / (r * (exp(r * t) - 1))
}

outbreak <- simulate_exponential_cases(sample_size = 100000)

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

cases <- construct_cases_by_obs_window(
  obs, windows = c(100)
)

plot_cases_by_obs_window(cases) +
  scale_y_log10()

censor_delay1 <- calculate_censor_delay(obs)

plot_censor_delay(censor_delay1) +
  geom_hline(yintercept = tfun(), lty = 2)

truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 30)


censor_delay <- calculate_censor_delay(truncated_obs)

plot_censor_delay(censor_delay)  +
  geom_hline(yintercept = tfun(), lty = 2)

cases2 <- construct_cases_by_obs_window(
  obs, windows = c(30)
)

plot_cases_by_obs_window(cases2)

outbreak2 <- simulate_exponential_cases(r = 0, sample_size = 100000)

obs2 <- outbreak2 |>
  simulate_secondary(
    meanlog = secondary_dist$meanlog[[1]],
    sdlog = secondary_dist$sdlog[[1]]
  ) |>
  observe_process()

truncated_obs2 <- obs2 |>
  filter_obs_by_obs_time(obs_time = 30)


censor_delay2 <- calculate_censor_delay(truncated_obs2)

plot_censor_delay(censor_delay2)


censor_delay2 <- calculate_censor_delay(obs2)

plot_censor_delay(censor_delay2)
