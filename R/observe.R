#' Observation process for primary and secondary events
#' @export
observe_process <- function(linelist) {
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
    # previously: delay_daily=floor(delay)
    DT(, delay_daily := stime_daily - ptime_daily) |>
    DT(, delay_lwr := purrr::map_dbl(delay_daily, ~ max(0, . - 1))) |>
    DT(, delay_upr := delay_daily + 1) |>
    # We assume observation time is the ceiling of the maximum delay
    DT(, obs_at := stime |>
        max() |>
        ceiling()
    )
  return(clinelist)
}

#' Filter observations based on a observation time of secondary events
#' @export
filter_obs_by_obs_time <- function(linelist, obs_time) {
  truncated_linelist <- linelist |>
    data.table::copy() |>
    # Update observation time by when we are looking
    DT(, obs_at := obs_time) |>
    DT(, obs_time := obs_time - ptime) |>
    # I've assumed truncation in the middle of the censoring window.
    # For discussion.
    DT(, censored_obs_time := obs_at - (ptime_daily + 0.5)) |>
    DT(, censored := "interval") |>
    DT(stime <= obs_at)
  return(truncated_linelist)
}