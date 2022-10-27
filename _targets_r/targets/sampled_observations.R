tar_target(
  sampled_observations,
  group_truncated_obs |>
    as.data.table() |>
    DT(sample(1:.N, sample_sizes, replace = FALSE)) |>
    DT(, sample_size := as.factor(sample_sizes)),
  pattern = cross(sample_sizes, group_truncated_obs)
)
