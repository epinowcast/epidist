censor_primary <- function(linelist) {
  clinelist <- linelist |>
    data.table::copy() |>
    DT(, ptime_daily := floor(ptime)) |>
    DT(, ptime_lwr := ptime_daily) |>
    DT(, ptime_upr := ptime_daily + 1)
  return(clinelist)
}

simulate_secondary <- function(linelist, dist = rlnorm, estimation_time,  ...) {
  obs <- linelist |>
    data.table::copy() |>
    DT(, delay := dist(.N, ...)) |>
    # When the second event actually happens
    DT(, stime := ptime + delay) |>
    # How the second event would be recorded in the data
    DT(, stime_daily := floor(stime)) |>
    DT(, stime_lwr := stime_daily) |>
    DT(, stime_upr := stime_daily + 1) |>
    # Time observe for
    DT(, obs_time := estimation_time - ptime) |>
    DT(, censored_obs_time := estimation_time - ptime_daily)
    DT(, censored := "interval")
  return(obs)
}

filter_for_observed_secondary <- function(linelist, estimation_time) {
  truncated_obs <- obs  |>
    data.table::copy() |>
    DT(stime <= estimation_time)
  return(truncated_obs)
}