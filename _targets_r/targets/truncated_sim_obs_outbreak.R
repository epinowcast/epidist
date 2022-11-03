tar_target(
  truncated_sim_obs_outbreak,
  simulated_observations_outbreak |>
    filter_obs_by_obs_time(
      obs_time = outbreak_estimation_times[, "time"][[1]]
    ) |>
    DT(, scenario := outbreak_estimation_times[, "scenario"][[1]]),
  pattern = map(outbreak_estimation_times)
)
