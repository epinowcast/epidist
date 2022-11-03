tar_target(simulated_scenarios_outbreak, {
  sampled_simulated_observations_outbreak |>
    DT(, .(scenario, distribution, sample_size, data_type)) |>
    unique() |>
    DT(, id := 1:.N)
})
