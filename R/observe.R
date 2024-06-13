#' Observation process for primary and secondary events
#'
#' @param linelist ...
#' @family observe
#' @autoglobal
#' @export
observe_process <- function(linelist) {
  clinelist <- data.table::copy(linelist)
  clinelist[, ptime_daily := floor(ptime)]
  clinelist[, ptime_lwr := ptime_daily]
  clinelist[, ptime_upr := ptime_daily + 1]
  # How the second event would be recorded in the data
  clinelist[, stime_daily := floor(stime)]
  clinelist[, stime_lwr := stime_daily]
  clinelist[, stime_upr := stime_daily + 1]
  # How would we observe the delay distribution
  # previously delay_daily would be the floor(delay)
  clinelist[, delay_daily := stime_daily - ptime_daily]
  clinelist[, delay_lwr := purrr::map_dbl(delay_daily, ~ max(0, . - 1))]
  clinelist[, delay_upr := delay_daily + 1]
  # We assume observation time is the ceiling of the maximum delay
  clinelist[, obs_at := stime |>
              max() |>
              ceiling()]

  return(clinelist)
}

#' Filter observations based on a observation time of secondary events
#'
#' @param linelist ...
#' @param obs_time ...
#' @family observe
#' @autoglobal
#' @export
filter_obs_by_obs_time <- function(linelist, obs_time) {
  truncated_linelist <- data.table::copy(linelist)
  truncated_linelist[, obs_at := obs_time]
  truncated_linelist[, obs_time := obs_time - ptime]
  truncated_linelist[, censored_obs_time := obs_at - ptime_lwr]
  truncated_linelist[, censored := "interval"]
  truncated_linelist <- truncated_linelist[stime_upr <= obs_at]

  return(truncated_linelist)
}

#' Filter observations based on the observation time of primary events
#'
#' @param linelist ...
#' @param obs_time ...
#' @param obs_at ...
#' @family observe
#' @autoglobal
#' @export
filter_obs_by_ptime <- function(linelist, obs_time,
                                obs_at = c("obs_secondary", "max_secondary")) {
  obs_at <- match.arg(obs_at)

  pfilt_t <- obs_time
  truncated_linelist <- data.table::copy(linelist)
  truncated_linelist[, censored := "interval"]
  truncated_linelist <- truncated_linelist[ptime_upr <= pfilt_t]

  if (obs_at == "obs_secondary") {
    # Update observation time to be the same as the maximum secondary time
    truncated_linelist[, obs_at := stime_upr]
  } else if (obs_at == "max_secondary") {
    truncated_linelist[, obs_at := stime_upr |> max() |> ceiling()]
  }

  # make observation time as specified
  truncated_linelist[, obs_time := obs_at - ptime]
  # Assuming truncation at the beginning of the censoring window
  truncated_linelist[, censored_obs_time := obs_at - ptime_lwr]

  # set observation time to artifial observation time
  if (obs_at == "obs_secondary") {
    truncated_linelist[, obs_at := pfilt_t]
  }
  return(truncated_linelist)
}

#' Pad zero observations as unstable in a lognormal distribution
#'
#' @param data ...
#' @param pad ...
#' @family observe
#' @autoglobal
#' @export
pad_zero <- function(data, pad = 1e-3) {
  data <- data.table::copy(data)
  # Need upper bound to be greater than lower bound
  data[censored_obs_time == 0, censored_obs_time := 2 * pad]
  data[delay_lwr == 0, delay_lwr := pad]
  data[delay_daily == 0, delay_daily := pad]
  return(data)
}

#' Drop zero observations as unstable in a lognormal distribution
#'
#' @param data ...
#' @family observe
#' @export
drop_zero <- function(data) {
  data <- data.table::copy(data)
  data <- data[delay_daily != 0]
  return(data)
}
