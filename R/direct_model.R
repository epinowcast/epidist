#' Prepare direct model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @family direct_model
#' @export
as_direct_model <- function(data) {
  UseMethod("as_direct_model")
}

assert_direct_model_input <- function(data) {
  assert_data_frame(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime"))
  assert_integer(data$case, lower = 0)
  assert_numeric(data$ptime, lower = 0)
  assert_numeric(data$stime, lower = 0)
}

#' Prepare latent individual model
#'
#' This function prepares data for use with the direct model. It does this by
#' adding columns used in the model to the `data` object provided. To do this,
#' the `data` must already have columns for the case number (integer),
#' (positive, numeric) times for the primary and secondary event times. The
#' output of this function is a `epidist_direct_model` class object, which may
#' be passed to [epidist()] to perform inference for the model.
#'
#' @param data A `data.frame` containing line list data
#' @rdname as_direct_model
#' @method as_direct_model data.frame
#' @family direct_model
#' @autoglobal
#' @export
as_direct_model.data.frame <- function(data) {
  assert_direct_model_input(data)
  class(data) <- c("epidist_direct_model", class(data))
  data <- data |>
    mutate(delay = .data$stime - .data$ptime)
  epidist_validate_model(data)
  return(data)
}

#' Validate direct model data
#'
#' This function checks whether the provided `data` object is suitable for
#' running the direct model. As well as making sure that
#' `is_direct_model()` is true, it also checks that `data` is a `data.frame`
#' with the correct columns.
#'
#' @param data A `data.frame` containing line list data
#' @param ... ...
#' @method epidist_validate_model epidist_direct_model
#' @family direct_model
#' @export
epidist_validate_model.epidist_direct_model <- function(data, ...) {
  assert_true(is_direct_model(data))
  assert_direct_model_input(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime", "delay"))
  assert_numeric(data$delay, lower = 0)
}

#' Check if data has the `epidist_direct_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family latent_individual
#' @export
is_direct_model <- function(data) {
  inherits(data, "epidist_direct_model")
}
