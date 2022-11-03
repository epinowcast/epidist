tar_target(simulated_scenarios_exponential, {
  sampled_simulated_observations_exponential |>
    DT(, .(scenario, distribution, sample_size, data_type, replicate)) |>
    unique() |>
    DT(, id := 1:.N)
})
