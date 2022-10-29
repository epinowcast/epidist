tar_target(
  truncated_sim_obs_exponential,
  simulated_observations_exponential |>
    filter_obs_by_obs_time(obs_time = 30) |>
    DT(, estimation_time := 30)
)
