library(data.table) # here we use the development version of data.table install it with data.table::update_dev_pkg
library(purrr)
source("param.R")

samples <- 400
max_t <- 20
growth_rate_vec <- seq(-0.2, 0.2, length.out=11)

# first time using the data.table package so probably a bit rough...
# assuming infection time is ``exponentially'' distributed between 0 and max_t with rate -r...
# doing this so that we can keep exponential growth/decay while keeping the sample size fixed

linelist <- data.table(
  id = rep(1:samples, length(growth_rate_vec)),
  quant=runif(samples * length(growth_rate_vec), 0, 1),
  r=rep(growth_rate_vec, each=samples)
) |>
  # drawing infection time for each person
  DT(, time := ifelse(r==0, quant * max_t, log(1 + quant * (exp(r * max_t) - 1))/r)) |>
  DT(, daily_time := floor(time)) |>
  DT(, day_after_time := ceiling(time))

cases <- linelist[, .(cases = .N), by = c("daily_time", "r")][order(r, daily_time)]

# if you set samples to a large value, you'll find that this works...
# plot(cases[r==0.2]$cases)

# we now have a doubly censored thing for both primary and secondary events
obs <- linelist |>
  DT(, delay := rlnorm(samples * length(growth_rate_vec), logmean, logsd)) |>
  # When would data be observed
  DT(, obs_delay := time + delay) |>
  # Integerise delay
  DT(, daily_delay := floor(obs_delay)) |>
  # Day after observations
  DT(, day_after_delay := ceiling(obs_delay)) |>
  # Time observe for
  DT(, obs_time := max_t - time) |>
  DT(, censored := "interval")

# Truncate observations
truncated_obs <- obs  |>
  DT(obs_delay <= max_t) ## let's not worry about double censoring for now

save.image(file="data_exponential.rda")
