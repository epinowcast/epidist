#' Create an epidist_aggregate_data object
#'
#' @inheritParams as_epidist_linelist_data
#' @family aggregate_data
#' @export
as_epidist_aggregate_data <- function(data, ...) {
  UseMethod("as_epidist_aggregate_data")
}

#' Create an epidist_aggregate_data object from vectors of event times
#'
#' @inheritParams as_epidist_linelist_data.default
#' @inheritParams as_epidist_aggregate_data
#' @family aggregate_data
#' @autoglobal
#' @export
#' @examples
#' as_epidist_aggregate_data(
#'   data = c(1, 2, 3),
#'   ptime_upr = c(2, 3, 4),
#'   stime_lwr = c(3, 4, 5),
#'   stime_upr = c(4, 5, 6),
#'   obs_time = c(5, 6, 7),
#'   n = c(1, 2, 3)
#' )
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
#' @examples
#' sierra_leone_ebola_data |>
#'   dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
#'   as_epidist_aggregate_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested",
#'     n = "n"
#'   )
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

#' Convert linelist data to aggregate format
#'
#' This method aggregates an `epidist_linelist_data` object by counting unique
#' combinations of variables.
#'
#' @param by Character vector of additional variables to stratify by, beyond the
#'   required time variables.
#' @method as_epidist_aggregate_data epidist_linelist_data
#' @inheritParams as_epidist_aggregate_data
#' @family aggregate_data
#' @autoglobal
#' @export
#' @importFrom checkmate assert_character assert_names
#' @examples
#' # Default stratification by required time variables only
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data()
#'
#' # Additional stratification by other variables
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data(by = "age")
as_epidist_aggregate_data.epidist_linelist_data <- function(
    data, by = NULL, ...) {
  assert_epidist(data)

  # Required variables for epidist objects
  group_vars <- .linelist_required_cols()

  # Combine required variables with user-specified ones
  if (!is.null(by)) {
    assert_character(by)
    assert_names(names(data), must.include = by)
    group_vars <- c(group_vars, by)
  }

  agg <- data |>
    dplyr::count(across(all_of(group_vars)), name = "n")
  class(agg) <- setdiff(class(agg), "epidist_linelist_data")
  aggregated <- as_epidist_aggregate_data.default(
    data = agg$ptime_lwr,
    ptime_upr = agg$ptime_upr,
    stime_lwr = agg$stime_lwr,
    stime_upr = agg$stime_upr,
    obs_time = agg$obs_time,
    n = agg$n,
    ...
  )
  aggregated <- bind_cols(aggregated, agg[!names(agg) %in% names(aggregated)])
  return(aggregated)
}

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

#' Check if data has the `epidist_aggregate_data` class
#'
#' @inheritParams as_epidist_aggregate_data
#' @param ... Additional arguments
#' @family aggregate_data
#' @export
is_epidist_aggregate_data <- function(data, ...) {
  inherits(data, "epidist_aggregate_data")
}

#' Assert validity of `epidist_aggregate_data` objects
#'
#' @param data An object to check
#' @param ... Additional arguments
#' @method assert_epidist epidist_aggregate_data
#' @family aggregate_data
#' @export
assert_epidist.epidist_aggregate_data <- function(data, ...) {
  assert_epidist.epidist_linelist_data(data)
  assert_names(names(data), must.include = "n")
  assert_integerish(data$n, lower = 1)
  return(invisible(NULL))
}
