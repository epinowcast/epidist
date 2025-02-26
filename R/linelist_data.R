#' Create an epidist_linelist_data object
#'
#' Creates an epidist_linelist_data object from various input formats. This is
#' useful when working with individual-level data where each row represents a
#' single observation with primary and secondary event times. See the specific
#' methods for details on supported input formats and usage examples.
#'
#' @param data The data to convert
#'
#' @param ... Additional arguments passed to methods
#'
#' @family linelist_data
#' @export
as_epidist_linelist_data <- function(data, ...) {
  UseMethod("as_epidist_linelist_data")
}

#' Create an epidist_linelist_data object from vectors of event times
#'
#' This method takes vectors of event times (primary/secondary event times and
#' observation time) and creates an `epidist_linelist_data` object. This format
#' is useful when working with individual-level data where each row represents a
#' single observation. See the other methods for other data input options.
#'
#' @param data Numeric vector giving lower bounds for primary times.
#'
#' @param ptime_upr Numeric vector giving upper bounds for primary times.
#'
#' @param stime_lwr,stime_upr Numeric vectors giving lower and upper bounds for
#'  secondary times.
#'
#' @param obs_time Numeric vector giving observation times.
#'
#' @param ... Additional columns to add to the epidist_linelist_data object.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @family linelist_data
#' @export
#' @examples
#' as_epidist_linelist_data(
#'   data = c(1, 2, 3),
#'   ptime_upr = c(2, 3, 4),
#'   stime_lwr = c(3, 4, 5),
#'   stime_upr = c(4, 5, 6),
#'   obs_time = c(5, 6, 7)
#' )
as_epidist_linelist_data.default <- function(
  data,
  ptime_upr = NULL,
  stime_lwr = NULL,
  stime_upr = NULL,
  obs_time = NULL,
  ...
) {
  data_frame <- tibble(
    ptime_lwr = data,
    ptime_upr = ptime_upr,
    stime_lwr = stime_lwr,
    stime_upr = stime_upr,
    obs_time = obs_time
  )

  extra_cols <- list(...)
  if (length(extra_cols) > 0) {
    extra_cols <- extra_cols[!names(extra_cols) %in% names(data_frame)]
    if (length(extra_cols) > 0) {
      data_frame <- bind_cols(data_frame, extra_cols)
    }
  }

  data_frame <- new_epidist_linelist_data(data_frame)
  assert_epidist(data_frame)
  return(data_frame)
}

#' Create an epidist_linelist_data object from a data frame with event dates
#'
#' This method takes a data.frame containing event dates (primary/secondary
#' event dates and observation date) and creates an `epidist_linelist_data`
#' object. This format is useful when working with individual-level data where
#' each row represents a single observation. Internally it converts dates to
#' numeric times relative to the earliest primary event date and uses
#' [as_epidist_linelist_data.default()] to create the final object. See the
#' other methods for other data input options.
#'
#' @param data A data.frame containing line list data
#'
#' @param pdate_lwr A string giving the column of `data` containing the primary
#'  event lower bound as a datetime. Defaults to `NULL` which assumes that the
#'  variable `pdate_lwr` is present.
#'
#' @param pdate_upr A string giving the column of `data` containing the primary
#'  event upper bound as a datetime. If this column exists in the data it will
#'  be used, otherwise if not supplied then the value of `pdate_lwr` + 1 day is
#'  used.
#'
#' @param sdate_lwr A string giving the column of `data` containing the
#'  secondary event lower bound as a datetime. Defaults to `NULL` which assumes
#'  that the variable `sdate_lwr` is present.
#'
#' @param sdate_upr A string giving the column of `data` containing the
#'  secondary event upper bound as a datetime. If this column exists in the data
#'  it will be used, otherwise if not supplied then the value of `sdate_lwr` + 1
#'  day is used.
#'
#' @param obs_date A string giving the column of `data` containing the
#'  observation time as a datetime. Optional, if not supplied then the maximum
#'  of `sdate_upr` is used.
#'
#' @param ... Additional arguments passed to methods
#'
#' @family linelist_data
#' @importFrom dplyr bind_cols
#' @importFrom lubridate days is.timepoint
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom checkmate assert_true assert_names assert_numeric assert_date
#' @importFrom utils hasName
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   )
as_epidist_linelist_data.data.frame <- function(
  data,
  pdate_lwr = NULL,
  sdate_lwr = NULL,
  pdate_upr = NULL,
  sdate_upr = NULL,
  obs_date = NULL,
  ...
) {
  if (is.null(pdate_lwr) && !hasName(data, "pdate_lwr")) {
    cli::cli_abort("{.var pdate_lwr} is NULL but must be provided.")
  }

  if (is.null(sdate_lwr) && !hasName(data, "sdate_lwr")) {
    cli::cli_abort("{.var sdate_lwr} is NULL but must be provided.")
  }

  # Only include non-null inputs in renaming
  valid_inputs <- !sapply(
    list(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date),
    is.null
  )
  new_names <- .linelist_date_cols()
  old_names <- c(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date)
  data_tbl <- .rename_columns(
    data,
    new_names = new_names[valid_inputs],
    old_names = old_names
  )

  if (!hasName(data_tbl, "pdate_upr")) {
    cli::cli_alert_info(paste0(
      "No primary event upper bound provided, using the primary event lower ",
      "bound + 1 day as the assumed upper bound."
    ))
    data_tbl <- mutate(
      data_tbl,
      pdate_upr = .data$pdate_lwr + lubridate::days(1)
    )
  }

  if (!hasName(data_tbl, "sdate_upr")) {
    cli::cli_alert_info(paste0(
      "No secondary event upper bound provided, using the secondary event",
      " lower bound + 1 day as the assumed upper bound."
    ))
    data_tbl <- mutate(
      data_tbl,
      sdate_upr = .data$sdate_lwr + lubridate::days(1)
    )
  }

  if (!hasName(data_tbl, "obs_date")) {
    cli::cli_alert_info(paste0(
      "No observation time column provided, using ",
      max(data_tbl$sdate_upr),
      " as the observation date (the maximum of the secondary event upper ",
      "bound)."
    ))
    data_tbl <- mutate(data_tbl, obs_date = max(.data$sdate_upr, na.rm = TRUE))
  }

  col_names <- .linelist_date_cols()
  assert_names(names(data_tbl), must.include = col_names)

  assert_true(is.timepoint(data_tbl$pdate_lwr))
  assert_true(is.timepoint(data_tbl$pdate_upr))
  assert_true(is.timepoint(data_tbl$sdate_lwr))
  assert_true(is.timepoint(data_tbl$sdate_upr))
  assert_true(is.timepoint(data_tbl$obs_date))

  min_date <- min(data_tbl$pdate_lwr, na.rm = TRUE)

  # Convert to numeric times and use default method
  result <- as_epidist_linelist_data.default(
    data = as.numeric(data_tbl$pdate_lwr - min_date),
    ptime_upr = as.numeric(data_tbl$pdate_upr - min_date),
    stime_lwr = as.numeric(data_tbl$sdate_lwr - min_date),
    stime_upr = as.numeric(data_tbl$sdate_upr - min_date),
    obs_time = as.numeric(data_tbl$obs_date - min_date)
  )

  result <- bind_cols(result, data_tbl[!names(data_tbl) %in% names(result)])

  return(result)
}

#' Convert aggregate data to linelist format
#'
#' This method expands an `epidist_aggregate_data` object into individual
#' observations by uncounting the `n` column, then converts it to linelist
#' format using [as_epidist_linelist_data.data.frame()].
#'
#' @method as_epidist_linelist_data epidist_aggregate_data
#' @inheritParams as_epidist_linelist_data
#' @family linelist_data
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
#'   as_epidist_aggregate_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested",
#'     n = "n"
#'   ) |>
#'   as_epidist_linelist_data()
as_epidist_linelist_data.epidist_aggregate_data <- function(data, ...) {
  assert_epidist.epidist_aggregate_data(data)
  data <- tidyr::uncount(data, weights = .data$n, .remove = TRUE) |>
    dplyr::select(-"n")

  class(data) <- setdiff(class(data), "epidist_aggregate_data")
  return(as_epidist_linelist_data(data))
}

#' Class constructor for `epidist_linelist_data` objects
#'
#' @param data A data.frame to convert
#'
#' @returns An object of class `epidist_linelist_data`
#'
#' @family linelist_data
#' @export
new_epidist_linelist_data <- function(data) {
  class(data) <- c("epidist_linelist_data", class(data))
  return(data)
}

#' Check if data has the `epidist_linelist_data` class
#'
#' @inheritParams as_epidist_linelist_data
#'
#' @param ... Additional arguments
#'
#' @family linelist_data
#' @export
is_epidist_linelist_data <- function(data, ...) {
  return(inherits(data, "epidist_linelist_data"))
}

#' Assert validity of `epidist_linelist_data` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments
#'
#' @method assert_epidist epidist_linelist_data
#'
#' @family linelist_data
#' @export
assert_epidist.epidist_linelist_data <- function(data, ...) {
  assert_data_frame(data)
  col_names <- .linelist_required_cols()
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

.linelist_date_cols <- function() {
  return(c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"))
}

.linelist_required_cols <- function() {
  return(c("ptime_lwr", "ptime_upr", "stime_lwr", "stime_upr", "obs_time"))
}
