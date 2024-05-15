library(data.table)

set.seed(101)

meanlog <- 1.8
sdlog <- 0.5
obs_time <- 25
sample_size <- 200

example_obs <- simulate_gillespie() |>
  simulate_secondary(
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time) %>%
  .[sample(seq_len(.N), sample_size, replace = FALSE)]

usethis::use_data(example_obs, overwrite = TRUE, internal = TRUE)
