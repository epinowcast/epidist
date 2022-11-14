tar_target(ebola_scenarios, {
  sampled_ebola_observations |>
    DT(, .(scenario, obs_type, sample_size, data_type)) |>
    unique() |>
    DT(, id := 1:.N)
})
