#' For a target variable convert from individual data to counts
#'
#' @param linelist ...
#' @param target_time ...
#' @param additional_by ...
#' @param pad_zeros ...
#' @family preprocess
#' @autoglobal
#' @export
linelist_to_counts <- function(linelist, target_time = "ptime_daily",
                               additional_by = c(), pad_zeros = FALSE) {
  cases <- data.table::copy(linelist)
  cases[, time := get(target_time)]
  cases <- cases[, list(cases = .N), by = c("time", additional_by)]
  cases <- cases[order(time)]

  if (pad_zeros) {
    cases <- merge(
      cases,
      data.table::data.table(time = 1:max(linelist[[target_time]])),
      by = "time",
      all = TRUE
    )

    cases[is.na(cases), cases := 0]
  }

  return(cases[])
}

#' Convert primary and secondary observations to counts in long format
#'
#' @param linelist ...
#' @family preprocess
#' @autoglobal
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
#'
#' @param dt ...
#' @family preprocess
#' @autoglobal
#' @export
reverse_obs_at <- function(dt) {
  dt_rev <- data.table::copy(dt)

  dt_rev[, obs_at := factor(obs_at)]
  dt_rev[, obs_at := factor(obs_at, levels = rev(levels(obs_at)))]

  return(dt_rev)
}

#' Construct case counts by observation window based on secondary observations
#'
#' @param linelist ...
#' @param windows ...
#' @param obs_type ...
#' @param upper_window ...
#' @family preprocess
#' @autoglobal
#' @export
construct_cases_by_obs_window <- function(linelist, windows = c(25, 45),
                                          obs_type = c("stime", "ptime"),
                                          upper_window =
                                            max(linelist$stime_daily)) {
  lower_window <- c(0, windows)
  upper_window <- c(windows, upper_window)

  obs_type <- match.arg(obs_type)
  if (obs_type == "stime") {
    filter_fn <- function(dt, lw, uw) { # nolint
      filter_obs_by_obs_time(dt, obs_time = uw)[stime > lw]
    }
  }else {
    filter_fn <- function(dt, lw, uw) { # nolint
      filter_obs_by_ptime(dt, obs_time = uw)[ptime > lw]
    }
  }

  cases <- purrr::map2(
    lower_window, upper_window, ~ filter_fn(linelist, .x, .y)
  ) |>
    data.table::rbindlist()

  primary_cases <- cases |>
    linelist_to_counts(additional_by = "obs_at")

  secondary_cases <- cases |>
    linelist_to_counts(
      target_time = "stime_daily", additional_by = "obs_at"
    )

  cases <- rbind(
    primary_cases[, case_type := "primary"],
    secondary_cases[, case_type := "secondary"]
  )

  cases <- reverse_obs_at(cases)
  return(cases[])
}

#' Combine truncated and fully observed observations
#'
#' @param truncated_obs ...
#' @param obs ...
#' @family preprocess
#' @autoglobal
#' @export
combine_obs <- function(truncated_obs, obs) {
  rbind(
    truncated_obs,
    obs[, obs_at := max(stime_daily)],
    fill = TRUE
  ) |>
    reverse_obs_at()
}

#' Calculate the mean difference between continuous and discrete event time
#'
#' @param truncated_obs ...
#' @param additional_by ...
#' @family preprocess
#' @autoglobal
#' @export
calculate_censor_delay <- function(truncated_obs, additional_by = c()) {
  truncated_obs_psumm <- data.table::copy(truncated_obs)
  truncated_obs_psumm[, ptime_delay := ptime - ptime_daily]
  truncated_obs_psumm <- truncated_obs_psumm[, list(
    mean = mean(ptime_delay),
    lwr = ifelse(length(ptime_delay) > 1, t.test(ptime_delay)[[4]][1], 0),
    upr = ifelse(length(ptime_delay) > 1, t.test(ptime_delay)[[4]][2], 1)
  ),
  by = c("ptime_daily", additional_by)]
  truncated_obs_psumm[, lwr := ifelse(lwr < 0, 0, lwr)]
  truncated_obs_psumm[, upr := ifelse(upr > 1, 1, upr)]
  truncated_obs_psumm[, type := "ptime"]

  truncated_obs_ssumm <- data.table::copy(truncated_obs)
  truncated_obs_ssumm[, stime_delay := stime - stime_daily]
  truncated_obs_ssumm <- truncated_obs_ssumm[, list(
    mean = mean(stime_delay),
    lwr = ifelse(length(stime_delay) > 1, t.test(stime_delay)[[4]][1], 0),
    upr = ifelse(length(stime_delay) > 1, t.test(stime_delay)[[4]][2], 1)
  ),
  by = c("stime_daily", additional_by)]
  truncated_obs_ssumm[, lwr := ifelse(lwr < 0, 0, lwr)]
  truncated_obs_ssumm[, upr := ifelse(upr > 1, 1, upr)]
  truncated_obs_ssumm[, type := "stime"]

  names(truncated_obs_psumm)[1] <- names(truncated_obs_ssumm)[1] <- "cohort"

  censor_delay <- rbind(truncated_obs_psumm, truncated_obs_ssumm)

  return(censor_delay[])
}

#' Convert from event based to incidence based data
#'
#' @param data ...
#' @param by ...
#' @family preprocess
#' @autoglobal
#' @export
event_to_incidence <- function(data, by = c()) {
  dd <- data.table::copy(data)
  dd[, list(cases = .N), by = c("ptime_daily", by)]
  dd <- dd[order(ptime_daily)]
  setnames(dd, old = c("ptime_daily"), new = c("time"))
  return(dd)
}
