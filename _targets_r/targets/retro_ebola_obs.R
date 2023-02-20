tar_target(
  retrospective_ebola_obs,
  case_study_data |>
    filter_obs_by_ptime(
      obs_time = ebola_estimation_times[, "time"][[1]],
      obs_at = "max_secondary"
    ) |>
    DT(, scenario := ebola_estimation_times[, "scenario"][[1]]) |>
    DT(, obs_type := "retrospective") |> 
    DT(ptime_lwr >= ebola_estimation_times[, "time"][[1]] - 60),
  pattern = map(ebola_estimation_times)
)
