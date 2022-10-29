tar_target(
  simulated_cases_exponential, 
  simulate_exponential_cases(
    r=growth_rate[,"r"][[1]]
  ) |>
    DT(, r := growth_rate[,"r"][[1]]) |>
    DT(, scenario := growth_rate[,"scenario"][[1]]),
  pattern = map(growth_rate)
)
