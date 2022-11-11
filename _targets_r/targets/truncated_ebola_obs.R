tar_target(
  truncated_ebola_obs,
  case_study_data |>
    filter_obs_by_obs_time(
      obs_time = ebola_estimation_times[, "time"][[1]]
    ) |>
    DT(, scenario := ebola_estimation_times[, "scenario"][[1]]),
  pattern = map(ebola_estimation_times)
)
