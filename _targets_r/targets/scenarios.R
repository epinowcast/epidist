tar_target(scenarios, {
  rbind(simulated_scenarios_outbreak[, replicate := 1],
        simulated_scenarios_exponential
  ) |> 
    as.data.table() |> 
    DT(, id := 1:.N)
})
