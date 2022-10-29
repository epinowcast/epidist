tar_target(
  sampled_simulated_observations_exponential,
  group_sim_obs_exponential |>
    as.data.table() |>
    DT(sample(1:.N, min(.N, sample_sizes), replace = FALSE)) |>
    DT(, sample_size := as.factor(sample_sizes)) |>
    DT(, datatype := "exponential"),
  pattern = cross(sample_sizes, group_sim_obs_exponential)
)
