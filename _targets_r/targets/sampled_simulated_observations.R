tar_target(
  sampled_simulated_observations,
  group_truncated_sim_obs |>
    as.data.table() |>
    DT(sample(1:.N, min(.N, sample_sizes), replace = FALSE)) |>
    DT(, sample_size := as.factor(sample_sizes)) |>
    DT(, datatype := "outbreak"),
  pattern = cross(sample_sizes, group_truncated_sim_obs)
)
