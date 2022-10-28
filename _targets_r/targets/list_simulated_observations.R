tar_target(list_simulated_observations, {
  sampled_simulated_observations |>
    split(by = c("estimation_time", "distribution", "sample_size"))
})
