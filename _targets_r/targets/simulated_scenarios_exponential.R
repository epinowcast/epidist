tar_target(simulated_scenarios_exponential, {
  sampled_simulated_observations_exponential |>
    DT(, .(scenario, distribution, sample_size, datatype)) |>
    unique() |>
    DT(, id := 1:.N)
})
