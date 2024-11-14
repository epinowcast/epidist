#' Prepare naive model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @family naive_model
#' @export
as_naive_model <- function(data) {
  UseMethod("as_naive_model")
}

assert_naive_model_input <- function(data) {
  assert_data_frame(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime"))
  assert_integer(data$case, lower = 0)
  assert_numeric(data$ptime, lower = 0)
  assert_numeric(data$stime, lower = 0)
}

#' @method as_naive_model data.frame
#' @family naive_model
#' @autoglobal
#' @export
as_naive_model.data.frame <- function(data) {
  assert_naive_model_input(data)
  class(data) <- c("epidist_naive_model", class(data))
  data <- data |>
    mutate(delay = .data$stime - .data$ptime)
  epidist_validate_model(data)
  return(data)
}

#' @method epidist_validate_model epidist_naive_model
#' @family naive_model
#' @export
epidist_validate_model.epidist_naive_model <- function(data, ...) {
  assert_true(is_naive_model(data))
  assert_naive_model_input(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime", "delay"))
  assert_numeric(data$delay, lower = 0)
}

#' Check if data has the `epidist_naive_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family latent_individual
#' @export
is_naive_model <- function(data) {
  inherits(data, "epidist_naive_model")
}
