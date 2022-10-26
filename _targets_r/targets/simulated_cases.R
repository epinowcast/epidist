tar_target(simulated_cases, {
  data.table(
    case = 1:1000,
    ptime = runif(1000, 0, 60)
  )
})
