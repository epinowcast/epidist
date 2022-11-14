tar_target(list_ebola_observations, {
  sampled_ebola_observations |>
    split(by = c("scenario", "obs_type", "sample_size", "data_type"))
})
