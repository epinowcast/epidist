#' Add columns for interval censoring of primary and secondary events
#'
#' @param linelist ...
#' @param pwindow The primary censoring window.
#' @param swindow The secondary censoring window.
#' @family observe
#' @autoglobal
#' @export
observe_process <- function(linelist, pwindow = 1, swindow = 1) {
  assert_numeric(pwindow, lower = 0)
  assert_numeric(swindow, lower = 0)
  
  linelist |>
    mutate(
      ptime_lwr = .floor_mult(.data$ptime, f = pwindow),
      ptime_upr = .data$ptime_lwr + pwindow,
      stime_lwr = .floor_mult(.data$stime, f = pwindow),
      stime_upr = .data$stime_lwr + swindow,
      delay_lwr = pmax(0, .data$stime_lwr - .data$ptime_lwr - 1),
      delay_upr = .data$delay_lwr + pwindow + swindow
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
      obs_time = obs_time - .data$ptime,
      censored_obs_time = .data$obs_time - .data$ptime_lwr,
      censored = "interval"
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
