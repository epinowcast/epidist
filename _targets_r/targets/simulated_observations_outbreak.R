tar_target(simulated_observations_outbreak, {
  simulated_secondary_outbreak |>
    observe_process()
})
