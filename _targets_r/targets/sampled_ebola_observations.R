tar_target(
  sampled_ebola_observations,
  group_truncated_ebola_obs |>
    as.data.table() |>
    DT(, sample_size := .N) |>
    DT(, data_type := "ebola_case_study"),
  pattern = map(group_truncated_ebola_obs)
)
