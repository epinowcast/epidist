#' For a target variable convert from individual data to counts
#' @export
linelist_to_counts <- function(linelist, target_time = "ptime_daily",
                               additional_by = c(), pad_zeros = FALSE) {
  cases <- linelist |>
    data.table::copy() |>
    data.table::DT(, time := get(target_time)) |>
    data.table::DT(, .(cases = .N), by = c("time", additional_by)) |>
    data.table::DT(order(time))

  if (pad_zeros) {
    cases <- cases |>
      merge(
        data.table::data.table(time = 1:max(linelist[[target_time]])),
        by = "time",
        all = TRUE
      ) |>
      data.table::DT(is.na(cases), cases := 0)
  }
  return(cases[])
}

#' Convert primary and secondary observations to counts in long format
#' @export
linelist_to_cases <- function(linelist) {
  primary_cases <- linelist_to_counts(linelist)
  secondary_cases <- linelist_to_counts(linelist, "stime_daily")
  cases <- merge(
    primary_cases[, primary := cases][, cases := NULL],
    secondary_cases[, secondary := cases][, cases := NULL],
    by = "time", all = TRUE
  )
  cases[is.na(primary), primary := 0]
  cases[is.na(secondary), secondary := 0]
  cases <- data.table::melt(
    cases,
    measure.vars = c("primary", "secondary"),
    variable.name = "case_type", value.name = "cases"
  )
  return(cases[])
}

#' For the observation observed at variable reverse the factor ordering
#' @export
reverse_obs_at <- function(dt) {
  dt |>
    DT(, obs_at := factor(obs_at)) |>
    DT(, obs_at := factor(obs_at, levels = rev(levels(obs_at))))
}

#' Construct case counts by observation window based on secondary observations
#' @export
construct_cases_by_obs_window <- function(linelist, windows = c(25, 45)) {
  lower_window <- c(0, windows)
  upper_window <- c(windows, max(linelist$stime_daily))

  cases <- purrr::map2(
    lower_window, upper_window,
    ~ linelist |>
      filter_obs_by_obs_time(obs_time = .y) |>
      data.table::DT(stime > .x)
  ) |>
  data.table::rbindlist()

  primary_cases <- cases |>
    linelist_to_counts(additional_by = "obs_at")

  secondary_cases <- cases |>
    linelist_to_counts(target_time = "stime_daily", additional_by = "obs_at")

  cases <- rbind(
    primary_cases[, case_type := "primary"],
    secondary_cases[, case_type := "secondary"]
  )

  cases <- reverse_obs_at(cases)
  return(cases[])

}

#' Combine truncated and fully observed observations
#' @export
combine_obs <- function(truncated_obs, obs) {
  cobs <- rbind(
    truncated_obs,
    obs[, obs_at := max(stime_daily)],
    fill = TRUE
  ) |>
    reverse_obs_at()
}

#' Calculate the mean difference between continuous and discrete event time
#' @export
calculate_censor_delay <- function(truncated_obs) {
  truncated_obs_psumm <- truncated_obs |>
    copy() |>
    DT(, ptime_delay := ptime - ptime_daily) |>
    DT(, .(
      mean = mean(ptime_delay),
      lwr = ifelse(length(ptime_delay) > 1, t.test(ptime_delay)[[4]][1], 0),
      upr = ifelse(length(ptime_delay) > 1, t.test(ptime_delay)[[4]][2], 1)),
      by = "ptime_daily") |>
    DT(, lwr := ifelse(lwr < 0, 0, lwr)) |>
    DT(, upr := ifelse(upr > 1, 1, upr)) |>
    DT(, type := "ptime")

  truncated_obs_ssumm <- truncated_obs |>
    copy() |>
    DT(, stime_delay := stime - stime_daily) |>
    DT(, .(
      mean = mean(stime_delay),
      lwr = ifelse(length(stime_delay) > 1, t.test(stime_delay)[[4]][1], 0),
      upr = ifelse(length(stime_delay) > 1, t.test(stime_delay)[[4]][2], 1)),
      by = "stime_daily") |>
    DT(, lwr := ifelse(lwr < 0, 0, lwr)) |>
    DT(, upr := ifelse(upr > 1, 1, upr)) |>
    DT(, type := "stime")

  names(truncated_obs_psumm)[1] <- names(truncated_obs_ssumm)[1] <- "cohort"

  censor_delay <- rbind(truncated_obs_psumm, truncated_obs_ssumm)

  return(censor_delay[])
}
