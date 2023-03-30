tar_target(retro_outbreak_incidence, {
  tar_target(
    retro_outbreak_incidence,
    simulated_observations_outbreak |> 
      filter_obs_by_ptime(
          obs_time = outbreak_estimation_times[, "time"][[1]],
          obs_at = "max_secondary"
      ) |>
      event_to_incidence() |>
      DT(, obs_time := outbreak_estimation_times[, "time"][[1]]),
    pattern = map(outbreak_estimation_times)
  )
})
