tar_target(list_sampled_observations, {
  sampled_observations |>
    split(by = c("estimation_time", "distribution", "sample_size"))
})
