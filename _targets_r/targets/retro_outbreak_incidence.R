tar_target(
  retro_outbreak_incidence,
  simulated_observations_outbreak |> 
    filter_obs_by_ptime(
        obs_time = outbreak_estimation_times[, "time"][[1]],
        obs_at = "max_secondary"
    ) |>
    DT(, scenario := outbreak_estimation_times[, "scenario"][[1]]) |>
    event_to_incidence(c("scenario", "distribution")),
  pattern = map(outbreak_estimation_times)
)
