#' Observation process for primary and secondary events
#'
#' This function adds columns to linelist data representing an observation
#' process with daily primary and secondary censoring, as well as right
#' truncation. The columns added are:
#' * `ptime_daily`: The floor of `ptime`
#' * `ptime_lwr`: The lower bound of `ptime`. Equal to `ptime_daily`
#' * `ptime_upr`: The upper bound of `ptime`. Equal to `ptime_lwr + 1`
#' * `stime_daily`: The floor of `stime`
#' * `stime_lwr`: The lower bound of `stime`. Equal to `stime_daily`
#' * `stime_upr`: The upper bound of `stime`. Equal to `stime_lwr + 1`
#' * `delay_daily`: Given by `stime_daily - ptime_daily`
#' * `delay_lwr`: Given by `delay_daily - 1` (or 0 if `delay_daily < 1`)
#' * `delay_upr`: Given by `delay_daily + 1`
#' * `obs_at`: The maximum value of `stime`
#'
#' @param linelist ...
#' @family observe
#' @autoglobal
#' @export
observe_process <- function(linelist) {
  linelist |>
    dplyr::mutate(
      ptime_daily = floor(ptime),
      ptime_lwr = ptime_daily,
      ptime_upr = ptime_daily + 1,
      stime_daily = floor(stime),
      stime_lwr = stime_daily,
      stime_upr = stime_daily + 1,
      delay_daily = stime_daily - ptime_daily,
      delay_lwr = purrr::map_dbl(delay_daily, ~ max(0, . - 1)),
      delay_upr = delay_daily + 1,
      obs_at = ceiling(max(stime))
    )
}

#' Filter observations based on a observation time of secondary events
#'
#' @param linelist ...
#' @param obs_time ...
#' @family observe
#' @autoglobal
#' @export
filter_obs_by_obs_time <- function(linelist, obs_time) {
  linelist |>
    dplyr::mutate(
      obs_at = obs_time,
      obs_time = obs_time - ptime,
      censored_obs_time = obs_at - ptime_lwr,
      censored = "interval"
    ) |>
    dplyr::filter(stime_upr <= obs_at)
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
