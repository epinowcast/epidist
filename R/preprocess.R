#' Prepare date data in the `epidist_linelist` format
#'
#' @param data A `data.frame` containing line list data
#' @param pdate_lwr,pdate_upr,sdate_lwr,sdate_upr Strings giving the column of
#' `data` containing the primary and secondary event upper and lower bounds.
#' These columns of `data` must be as datetime.
#' @param obs_date A string giving the column of `data` containing the
#' observation time as a datetime.
#' @family preprocess
#' @export
as_epidist_linelist <- function(
    data, pdate_lwr = NULL, pdate_upr = NULL, sdate_lwr = NULL, sdate_upr = NULL,
    obs_date = NULL) {
  class(data) <- c("epidist_linelist", class(data))

  data <- .rename_columns(data,
    new_names = c(
      "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
    ),
    old_names = c(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date)
  )

  # Check for being a datetime
  assert_true(any(inherits(data$pdate_lwr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(data$pdate_upr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(data$sdate_lwr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(data$sdate_upr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(data$obs_date, c("POSIXct", "POSIXlt"))))

  # Convert datetime to time
  min_date <- min(data$pdate_lwr)

  data <- mutate(data,
    ptime_lwr = as.numeric(.data$pdate_lwr - min_date),
    ptime_upr = as.numeric(.data$pdate_upr - min_date),
    stime_lwr = as.numeric(.data$sdate_lwr - min_date),
    stime_upr = as.numeric(.data$sdate_upr - min_date),
    obs_time = as.numeric(.data$obs_date - min_date)
  )

  epidist_validate_data(data)

  return(data)
}

#' Validation for the `epidist_linelist` class
#'
#' @inheritParams as_epidist_linelist
#' @param ... Additional arguments
#' @family preprocess
#' @export
epidist_validate_data.epidist_linelist <- function(data, ...) {
  assert_true(is_epidist_linelist(data))
  assert_data_frame(data)
  col_names <- c(
    "case", "ptime_lwr", "ptime_upr", "stime_lwr", "stime_upr", "obs_time"
  )
  assert_names(names(data), must.include = col_names)
  assert_numeric(data$ptime_lwr, lower = 0)
  assert_numeric(data$ptime_upr, lower = 0)
  assert_true(all(data$ptime_upr - data$ptime_lwr > 0))
  assert_numeric(data$stime_lwr, lower = 0)
  assert_numeric(data$stime_upr, lower = 0)
  assert_true(all(data$stime_upr - data$stime_lwr > 0))
  assert_numeric(data$obs_time, lower = 0)
}

#' Check if data has the `epidist_linelist` class
#'
#' @inheritParams as_epidist_linelist
#' @param ... Additional arguments
#' @family preprocess
#' @export
is_epidist_linelist <- function(data, ...) {
  inherits(data, "epidist_linelist")
}
