tar_target(
  truncated_obs,
  simulated_observations |>
    filter_obs_by_obs_time(obs_time = estimation_times[, "time"][[1]]) |>
    DT(, estimation_time := estimation_times[, "scenario"][[1]]),
  pattern = map(estimation_times)
)
