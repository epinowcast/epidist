tar_target(
  simulated_secondary_outbreak, 
  simulated_cases_outbreak |>
    simulate_secondary(
      meanlog = distributions[, "meanlog"][[1]],
      sdlog = distributions[, "sdlog"][[1]]
    ) |>
    DT(, distribution := distributions[, "scenario"][[1]]),
  pattern = map(distributions)
)
