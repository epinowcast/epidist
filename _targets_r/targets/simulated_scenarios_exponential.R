tar_target(simulated_scenarios_exponential, {
  sampled_simulated_observations_exponential |>
    DT(, .(r, distribution, sample_size)) |>
    unique() |>
    DT(, id := 1:.N)
})
