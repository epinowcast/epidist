tar_target(simulated_scenarios, {
  sampled_simulated_observations |>
    DT(, .(estimation_time, distribution, sample_size)) |>
    unique() |>
    DT(, id := 1:.N)
})
