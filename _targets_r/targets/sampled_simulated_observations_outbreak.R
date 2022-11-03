tar_target(
  sampled_simulated_observations_outbreak,
  group_truncated_sim_obs_outbreak |>
    as.data.table() |>
    DT(sample(1:.N, min(.N, sample_sizes), replace = FALSE)) |>
    DT(, sample_size := as.factor(sample_sizes)) |>
    DT(, data_type := "outbreak"),
  pattern = cross(sample_sizes, group_truncated_sim_obs_outbreak)
)
