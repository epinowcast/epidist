tar_target(retro_exponential_incidence, {
  simulated_observations_exponential |> 
    filter_obs_by_ptime(
        obs_time = 30,
        obs_at = "max_secondary"
    ) |>
    event_to_incidence(by = c("r", "scenario", "distribution"))
})
