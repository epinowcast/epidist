tar_target(list_simulated_observations_outbreak, {
  sampled_simulated_observations_outbreak |>
    split(by = c("scenario", "distribution", "sample_size", "data_type"))
})
