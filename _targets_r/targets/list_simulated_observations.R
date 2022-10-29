tar_target(list_simulated_observations, {
  sampled_simulated_observations |>
    split(by = c("scenario", "distribution", "sample_size", "datatype"))
})
