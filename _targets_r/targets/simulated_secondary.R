tar_target(
  simulated_secondary, 
  simulated_cases |>
    simulate_secondary(
      meanlog = distributions[, "meanlog"][[1]],
      sdlog = distributions[, "sdlog"][[1]]
    ),
  pattern = map(distributions)
)
