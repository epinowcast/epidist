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

  cases[, obs_at := factor(obs_at)]
  cases[, obs_at := factor(obs_at, levels = rev(levels(obs_at)))]
  return(cases[])
}