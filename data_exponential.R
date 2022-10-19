library(data.table) # here we use the development version of data.table install it with data.table::update_dev_pkg
library(purrr)
source("param.R")
set.seed(101)

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
  # drawing primary time for each person
  DT(, ptime := ifelse(r==0, quant * max_t, log(1 + quant * (exp(r * max_t) - 1))/r)) |>
  # this represents when primary time is reported
  DT(, ptime_daily := floor(ptime)) |>
  DT(, ptime_lwr := ptime_daily) |>
  DT(, ptime_upr := ptime_daily+1)

cases <- linelist[, .(cases = .N), by = c("ptime_daily", "r")][order(r, ptime_daily)]

# if you set samples to a large value, you'll find that this works...
# plot(cases[r==0.2]$cases)

# we now have a doubly censored thing for both primary and secondary events
obs <- linelist |>
  DT(, delay := rlnorm(samples * length(growth_rate_vec), logmean, logsd)) |>
  # When the second event actually happens
  DT(, stime := ptime + delay) |>
  # How the second event would be recorded in the data
  DT(, stime_daily := floor(stime)) |>
  DT(, stime_lwr := stime_daily) |>
  DT(, stime_upr := stime_daily+1) |>
  # Time observe for
  DT(, obs_time := max_t - ptime) |>
  DT(, censored := "interval")

# Truncate observations
truncated_obs <- obs  |>
  DT(stime <= max_t)

save.image(file="data_exponential.rda")
