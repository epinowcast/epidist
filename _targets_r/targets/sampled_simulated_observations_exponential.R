tar_target(
  sampled_simulated_observations_exponential,
  group_sim_obs_exponential |>
    as.data.table() |>
    DT(sample(1:.N, min(.N, 200), replace = FALSE)) |>
    DT(, sample_size := as.factor(200)) |>
    DT(, data_type := "exponential") |>
    DT(, replicate := replicates_exponential),
  pattern = cross(
    sample_sizes, group_sim_obs_exponential, replicates_exponential
  )
)
