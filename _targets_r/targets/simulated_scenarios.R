tar_target(simulated_scenarios, {
  sampled_simulated_observations |>
    DT(, .(scenario, distribution, sample_size, datatype)) |>
    unique() |>
    DT(, id := 1:.N)
})
