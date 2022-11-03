tar_target(simulated_cases_outbreak, {
  simulate_gillespie(r = 0.2, gamma = 1 / 7, init_I = 50, n = 10000, seed = 101)
})
