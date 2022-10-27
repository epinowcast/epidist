tar_target(models, {
  list(
    "Naive" = naive_delay,
    "Censoring adjusted" = censoring_adjusted_delay,
    "Filtered" = function(data, ...) {
      data <- data |>
        data.table::copy() |>
        DT(stime_daily <= (obs_at - 15))
      naive_delay(data = data, ...)
    },
    "Filtered and censoring adjusted" = function(data, ...) {
      data <- data |>
        data.table::copy() |>
        DT(stime_daily <= (obs_at - 15))
      censoring_adjusted_delay(data = data, ...)
    },
    "Truncation adjusted" = truncation_adjusted_delay,
    "Truncation and censoring adjusted" = truncation_censoring_adjusted_delay,
    "Latent variable truncation and censoring adjusted" =
      latent_truncation_censoring_adjusted_delay
  )
})
