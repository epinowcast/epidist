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
#' * `obs_time`: The maximum value of `stime`
#'
#' @param linelist ...
#' @family observe
#' @autoglobal
#' @export
observe_process <- function(linelist) {
  linelist |>
    mutate(
      ptime_daily = floor(.data$ptime),
      ptime_lwr = .data$ptime_daily,
      ptime_upr = .data$ptime_daily + 1,
      stime_daily = floor(.data$stime),
      stime_lwr = .data$stime_daily,
      stime_upr = .data$stime_daily + 1,
      delay_daily = .data$stime_daily - .data$ptime_daily,
      delay_lwr = purrr::map_dbl(.data$delay_daily, ~ max(0, . - 1)),
      delay_upr = .data$delay_daily + 1,
      obs_time = ceiling(max(.data$stime))
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
    mutate(
      obs_time = obs_time,
      relative_obs_time = .data$obs_time - .data$ptime,
    ) |>
    filter(.data$stime_upr <= .data$obs_time)
}

#' Filter observations based on the observation time of primary events
#'
#' @param linelist ...
#' @param obs_time ...
#' @param obs_time_type ...
#' @family observe
#' @autoglobal
#' @export
filter_obs_by_ptime <- function(linelist, obs_time,
                                obs_time_type =
                                  c("obs_secondary", "max_secondary")) {
  obs_time <- match.arg(obs_time)
  pfilt_t <- obs_time
  truncated_linelist <- linelist |>
    mutate(censored = "interval") |>
    filter(.data$ptime_upr <= pfilt_t)
  if (obs_time_type == "obs_secondary") {
    # Update observation time to be the same as the maximum secondary time
    truncated_linelist <- mutate(truncated_linelist, obs_time = .data$stime_upr)
  } else if (obs_time_type == "max_secondary") {
    truncated_linelist <- truncated_linelist |>
      mutate(obs_time := .data$stime_upr |> max() |> ceiling())
  }
  # Make observation time as specified
  truncated_linelist <- truncated_linelist |>
    mutate(
      obs_time = .data$obs_time - .data$ptime,
      censored_obs_time = .data$obs_time - .data$ptime_lwr
    )
  # Set observation time to artificial observation time if needed
  if (obs_time_type == "obs_secondary") {
    truncated_linelist <- mutate(truncated_linelist, obs_time = pfilt_t)
  }
  return(truncated_linelist)
}
