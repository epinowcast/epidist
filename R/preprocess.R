add_event_vars <- function(
  data, ptime_lwr = NULL, pwindow = NULL, stime_lwr = NULL, swindow = NULL
) {
  data |>
    mutate(
      ptime_upr = !!ptime_lwr + !!pwindow,
      stime_upr = !!stime_lwr + !!swindow
    )
}

as_epidist_linelist <- function(
  data, ptime_lwr = NULL, pwindow = NULL, ptime_upr = NULL, stime_lwr = NULL,
  swindow = NULL, stime_upr = NULL
) {
  class(data) <- c("epidist_linelist", class(data))
  # this is inefficient and needs a refactor but it's a technical challenge
  data <- .rename_column(data, "ptime_lwr", ptime_lwr)
  data <- .rename_column(data, "ptime_upr", ptime_upr)
  data <- .rename_column(data, "stime_lwr", stime_lwr)
  data <- .rename_column(data, "stime_upr", stime_upr)
  data <- .rename_column(data, "pwindow", pwindow)
  data <- .rename_column(data, "swindow", swindow)
  return(data)
}

add_obs_vars <- function(
 data, obs_time, ptime_lwr = NULL
) {
  # obs_time could be numeric (same for all cases) or vector (different)
  # do we need to give the name for ptime_lwr here? no guaruntee of what it is
  data |>
    mutate(
      obs_time = obs_time,
      relative_obs_time = obs_time - ptime_lwr
    )
}
