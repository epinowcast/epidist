tar_target(scenarios, {
  sampled_observations |>
    DT(, .(estimation_time, distribution, sample_size)) |>
    unique() |>
    DT(, id := 1:.N)
})
