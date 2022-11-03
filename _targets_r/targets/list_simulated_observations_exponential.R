tar_target(list_simulated_observations_exponential, {
  sampled_simulated_observations_exponential |>
    split(
      by = c("scenario", "distribution", "sample_size", "data_type", "replicate")
    )
})
