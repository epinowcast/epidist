tar_target(scenarios, {
  rbind(
    simulated_scenarios_outbreak[, replicate := 1],
    simulated_scenarios_exponential,
    ebola_scenarios
    fill = TRUE
  ) |> 
    as.data.table() |> 
    DT(, id := 1:.N)
})
