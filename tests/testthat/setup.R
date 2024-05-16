set.seed(101)

meanlog <- 1.8
sdlog <- 0.5
obs_time <- 25
sample_size <- 200

obs <- simulate_gillespie() |>
  simulate_secondary(
    meanlog = meanlog,
    sdlog = sdlog
  )

obs_cens <- observe_process(obs)

obs_cens_trunc <- filter_obs_by_stime(obs_cens, obs_time = obs_time)

obs_cens_trunc_samp <- obs_cens_trunc[sample(seq_len(.N), sample_size, replace = FALSE)]
