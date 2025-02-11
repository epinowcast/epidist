#' Prepare naive model to pass through to `brms`
#'
#' @param data An object to be converted to the class `epidist_naive_model`.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @family naive_model
#' @export
as_epidist_naive_model <- function(data, ...) {
  UseMethod("as_epidist_naive_model")
}

#' The naive model method for `epidist_linelist_data` objects
#'
#' This method converts linelist data to a naive model format by calculating
#' delays between primary and secondary events. If the input data contains an
#' `n` column (e.g. from aggregated data), the likelihood will be weighted by
#' these counts.
#'
#' @param data An `epidist_linelist_data` object.
#'
#' @inheritParams .add_weights
#' @inheritParams as_epidist_naive_model
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
as_epidist_naive_model.epidist_linelist_data <- function(
    data, weight = NULL, ...) {
  assert_epidist.epidist_linelist_data(data)

  data <- data |>
    mutate(delay = .data$stime_lwr - .data$ptime_lwr)

  data <- .add_weights(data, weight)

  data <- new_epidist_naive_model(data)
  assert_epidist(data)
  return(data)
}

#' The naive model method for `epidist_aggregate_data` objects
#'
#' This method converts aggregate data to a naive model format by passing it to
#' [as_epidist_naive_model.epidist_linelist_data()] with the `n` column used as
#' weights. This ensures that the likelihood is weighted by the counts in the
#' aggregate data.
#'
#' @param data An `epidist_aggregate_data` object.
#'
#' @inheritParams as_epidist_naive_model.epidist_linelist_data
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
as_epidist_naive_model.epidist_aggregate_data <- function(data, ...) {
  return(as_epidist_naive_model.epidist_linelist_data(
    data,
    weight = "n",
    ...
  ))
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
  return(inherits(data, "epidist_naive_model"))
}

#' @method assert_epidist epidist_naive_model
#' @family naive_model
#' @export
assert_epidist.epidist_naive_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = "delay")
  assert_numeric(data$delay, lower = 0)
  return(invisible(NULL))
}

#' Define the model-specific component of an `epidist` custom formula for the
#' naive model
#'
#' @inheritParams epidist_formula_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_formula_model epidist_naive_model
#'
#' @family naive_model
#' @export
epidist_formula_model.epidist_naive_model <- function(
    data, formula, ...) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula, delay | weights(n) ~ .
  )
  return(formula)
}
