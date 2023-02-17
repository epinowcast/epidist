library(dynamicaltruncation)
library(data.table)
library(cmdstanr)
library(ggplot2)

outbreak <- simulate_gillespie(seed=101)

obs <- outbreak |>
  simulate_secondary(
  ) |>
  observe_process()

obs_window <- 30

truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = obs_window )

cases_by_window <- construct_cases_by_obs_window(
  obs, windows = c(obs_window)
)

cases_by_window |> 
  plot_cases_by_obs_window()

truncated_cases <- cases_by_window |> 
  DT(as.numeric(as.character(obs_at)) <= obs_window) |>
  DT(case_type == "primary")

cases <- data.frame(
  time = 0:obs_window,
  cases = 1e-3
)

cases$cases[match(truncated_cases$time, cases$time)]<- truncated_cases$cases

model <- cmdstan_model("data/models/lognormal_dynamical_full.stan")

truncated_obs_dropzero <-  truncated_obs |>
  DT(delay_daily != 0)

standata <- list(
  N = nrow(truncated_obs_dropzero),
  delay_daily = truncated_obs_dropzero$delay_daily,
  stime_daily = truncated_obs_dropzero$stime_daily,
  tlength = nrow(cases),
  tmin = min(cases$time),
  incidence_p = cases$cases
)

mysample <- model$sample(data = standata, cores = 4)

backward <- truncated_obs |>
  copy() |>
  DT(, .(mean = mean(delay_daily)), by = stime_daily)

backward2 <- obs |>
  copy() |>
  DT(, .(mean = mean(delay_daily)), by = stime_daily)

ss <- mysample$summary()

ss2 <- ss |>
  as.data.table() |>
  DT(grepl("backward", variable)) |>
  DT(, time := standata$tmin:(standata$tlength-1))

ggplot(backward) +
  geom_point(aes(stime_daily, mean)) +
  geom_ribbon(data = ss2, aes(time, ymin = q5, ymax = q95), alpha = 0.3) +
  geom_line(data = ss2, aes(time, mean))
