#' Prepare naive model to pass through to `brms`
#'
#' @param data An object to be converted to the class `epidist_naive_model`.
#'
#' @family naive_model
#' @export
as_epidist_naive_model <- function(data) {
  UseMethod("as_epidist_naive_model")
}

#' The naive model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object.
#'
#' @method as_epidist_naive_model epidist_linelist_data
#'
#' @family naive_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_naive_model()
as_epidist_naive_model.epidist_linelist_data <- function(data) {
  assert_epidist.epidist_linelist_data(data)

  data <- data |>
    mutate(delay = .data$stime_lwr - .data$ptime_lwr)

  data <- new_epidist_naive_model(data)
  assert_epidist(data)
  return(data)
}

#' The naive model method for `epidist_aggregate_data` objects
#'
#' @param data An `epidist_aggregate_data` object.
#'
#' @method as_epidist_naive_model epidist_aggregate_data
#'
#' @family naive_model
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
#'   as_epidist_naive_model()
as_epidist_naive_model.epidist_aggregate_data <- function(data) {
  linelist_data <- as_epidist_linelist_data.epidist_aggregate_data(data)

  as_epidist_naive_model(linelist_data)
}

#' Class constructor for `epidist_naive_model` objects
#'
#' @param data An object to be set with the class `epidist_naive_model`.
#'
#' @returns An object of class `epidist_naive_model`.
#'
#' @family naive_model
#' @export
new_epidist_naive_model <- function(data) {
  class(data) <- c("epidist_naive_model", class(data))
  return(data)
}

#' Check if data has the `epidist_naive_model` class
#'
#' @param data An object.
#'
#' @family naive_model
#' @export
is_epidist_naive_model <- function(data) {
  inherits(data, "epidist_naive_model")
}

#' @method assert_epidist epidist_naive_model
#' @family naive_model
#' @export
assert_epidist.epidist_naive_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = "delay")
  assert_numeric(data$delay, lower = 0)
}
