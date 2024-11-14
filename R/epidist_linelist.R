#' Create an epidist_linelist object
#'
#' @param data The data to convert
#' @param ... Additional arguments passed to methods
#' @family preprocess
#' @export
as_epidist_linelist <- function(data, ...) {
  UseMethod("as_epidist_linelist")
}

#' Create an epidist_linelist object from vectors of event times
#'
#' @param data Numeric vector giving lower bounds for primary times
#' @param ptime_upr Numeric vector giving upper bounds for primary times
#' @param stime_lwr,stime_upr Numeric vectors giving lower and upper bounds for
#' secondary times
#' @param obs_time Numeric vector giving observation times
#' @param ... Additional columns to add to the epidist_linelist object
#' @export
as_epidist_linelist.default <- function(
  data, ptime_upr = NULL, stime_lwr = NULL, stime_upr = NULL,
  obs_time = NULL, ...
) {
  # Create base data frame with required columns
  df <- data.frame(
    ptime_lwr = data,
    ptime_upr = ptime_upr,
    stime_lwr = stime_lwr,
    stime_upr = stime_upr,
    obs_time = obs_time
  )

  # Add any additional columns passed via ...
  extra_cols <- list(...)
  if (length(extra_cols) > 0) {
    df <- cbind(df, extra_cols)
  }

  df <- new_epidist_linelist(df)
  assert_epidist(df)

  return(df)
}

#' Create an epidist_linelist object from a data frame with event dates
#'
#' @param data A data.frame containing line list data
#' @param pdate_lwr,pdate_upr,sdate_lwr,sdate_upr Strings giving the column of
#' `data` containing the primary and secondary event upper and lower bounds.
#' These columns of `data` must be as datetime.
#' @param obs_date A string giving the column of `data` containing the
#' observation time as a datetime.
#' @param ... Additional arguments passed to methods
#' @family preprocess
#' @export
as_epidist_linelist.data.frame <- function(
  data, pdate_lwr = NULL, pdate_upr = NULL, sdate_lwr = NULL, sdate_upr = NULL,
  obs_date = NULL, ...
) {
  df <- .rename_columns(data,
    new_names = c(
      "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
    ),
    old_names = c(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date)
  )

  col_names <- c(
    "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
  )
  assert_names(names(df), must.include = col_names)

  # Check for being a datetime
  assert_true(any(inherits(df$pdate_lwr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(df$pdate_upr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(df$sdate_lwr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(df$sdate_upr, c("POSIXct", "POSIXlt"))))
  assert_true(any(inherits(df$obs_date, c("POSIXct", "POSIXlt"))))

  # Convert datetime to time
  min_date <- min(df$pdate_lwr)

  # Convert to numeric times and use default method
  result <- as_epidist_linelist.default(
    data = as.numeric(df$pdate_lwr - min_date),
    ptime_upr = as.numeric(df$pdate_upr - min_date),
    stime_lwr = as.numeric(df$sdate_lwr - min_date),
    stime_upr = as.numeric(df$sdate_upr - min_date),
    obs_time = as.numeric(df$obs_date - min_date)
  )

  return(result)
}

#' Class constructor for `epidist_linelist` objects
#'
#' @param data A data.frame to convert
#' @returns An object of class `epidist_linelist`
#' @keywords internal
#' @export
new_epidist_linelist <- function(data) {
  class(data) <- c("epidist_linelist", class(data))
  return(data)
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

#' @rdname assert_epidist
#' @export
assert_epidist.epidist_linelist <- function(data, ...) {
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

  return(invisible(NULL))
}
