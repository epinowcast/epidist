tar_target(scenarios, {
  rbind(simulated_scenarios, simulated_scenarios_exponential) |>
    as.data.table() |> 
    DT(, id := 1:.N)
})
