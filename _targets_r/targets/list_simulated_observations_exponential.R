tar_target(list_simulated_observations_exponential, {
  sampled_simulated_observations_exponential |>
    split(by = c("r", "distribution", "sample_size"))
})
