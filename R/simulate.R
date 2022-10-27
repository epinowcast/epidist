simulate_secondary <- function(linelist, dist = rlnorm, ...) {
  obs <- linelist |>
    data.table::copy() |>
    DT(, delay := dist(.N, ...)) |>
    # When the second event actually happens
    DT(, stime := ptime + delay)
  return(obs)
}

simulate_observations <- function(linelist) {
  clinelist <- linelist |>
    data.table::copy() |>
    DT(, ptime_daily := floor(ptime)) |>
    DT(, ptime_lwr := ptime_daily) |>
    DT(, ptime_upr := ptime_daily + 1) |>
    # How the second event would be recorded in the data
    DT(, stime_daily := floor(stime)) |>
    DT(, stime_lwr := stime_daily) |>
    DT(, stime_upr := stime_daily + 1) |>
    # How would we observe the delay distribution
    DT(, delay_daily := floor(delay)) |>
    DT(, delay_lwr := max(0, delay_daily - 1)) |>
    DT(, delay_upr := delay_daily + 1)
  return(clinelist)
}

filter_obs_by_obs_time <- function(linelist, obs_time) {
  truncated_linelist <- linelist |>
    data.table::copy() |>
    DT(, obs_time := obs_time - ptime) |>
    DT(, censored_obs_time := obs_time - ptime_daily) |>
    DT(, censored := "interval") |>
    DT(stime <= obs_time)
  return(truncated_linelist)
}