#' Class constructor for `epidist_aggregate_data` objects
#'
#' @param data A data.frame to convert
#' @returns An object of class `epidist_aggregate_data`
#' @family aggregate_data
#' @export
new_epidist_aggregate_data <- function(data) {
  class(data) <- c("epidist_aggregate_data", class(data))
  return(data)
}

#' Create an epidist_aggregate_data object
#'
#' @inheritParams as_epidist_linelist_data
#' @param n Numeric vector of counts for each row
#' @family aggregate_data
#' @export
as_epidist_aggregate_data <- function(data, n = NULL, ...) {
  UseMethod("as_epidist_aggregate_data")
}

#' Create an epidist_aggregate_data object from vectors of event times
#'
#' @inheritParams as_epidist_linelist_data.default
#' @inheritParams as_epidist_aggregate_data
#' @family aggregate_data
#' @autoglobal
#' @export
as_epidist_aggregate_data.default <- function(
    data, n = NULL, ptime_upr = NULL, stime_lwr = NULL,
    stime_upr = NULL, obs_time = NULL, ...) {
  # Create linelist data first
  df <- as_epidist_linelist_data(
    data = data,
    ptime_upr = ptime_upr,
    stime_lwr = stime_lwr,
    stime_upr = stime_upr,
    obs_time = obs_time,
    ...
  )

  if (!is.null(n)) {
    df$n <- n
  } else {
    cli::cli_abort("{.var n} is NULL but must be provided.")
  }
  df <- new_epidist_aggregate_data(df)
  assert_epidist(df)
  return(df)
}

#' Create an epidist_aggregate_data object from a data.frame
#'
#' @param n A character string giving the name of the column containing the
#' counts for each row. If `NULL` then the column `n` must be present in the
#' data.
#' @inheritParams as_epidist_linelist_data.data.frame
#' @family aggregate_data
#' @autoglobal
#' @export
as_epidist_aggregate_data.data.frame <- function(
    data, n = NULL, pdate_lwr = NULL, sdate_lwr = NULL,
    pdate_upr = NULL, sdate_upr = NULL, obs_date = NULL, ...) {
  # First convert to linelist data
  df <- as_epidist_linelist_data.data.frame(
    data = data,
    pdate_lwr = pdate_lwr,
    sdate_lwr = sdate_lwr,
    pdate_upr = pdate_upr,
    sdate_upr = sdate_upr,
    obs_date = obs_date,
    ...
  )

  # Handle n column
  if (is.null(n)) {
    if (!hasName(data, "n")) {
      cli::cli_abort("{.var n} is NULL but must be provided.")
    }
    n <- "n"
  }

  df$n <- data[[n]]

  df <- new_epidist_aggregate_data(df)
  assert_epidist(df)
  return(df)
}

#' @method assert_epidist epidist_aggregate_data
#' @export
assert_epidist.epidist_aggregate_data <- function(data, ...) {
  NextMethod() # Call linelist assert first
  assert_names(names(data), must.include = "n")
  assert_integerish(data$n, lower = 1)
  return(invisible(NULL))
}
