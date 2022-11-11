tar_target(
  sampled_ebola_observations,
  group_truncated_ebola_obs |>
    as.data.table() |>
    DT(sample(1:.N, min(.N, 200), replace = FALSE)) |>
    DT(, sample_size := 200) |>
    DT(, data_type := "ebola_case_study"),
  pattern = map(group_truncated_ebola_obs)
)
