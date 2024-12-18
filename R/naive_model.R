#' Prepare naive model to pass through to `brms`
#'
#' @param data An object to be converted to the class `epidist_naive_model`
#' @family naive_model
#' @export
as_epidist_naive_model <- function(data) {
  UseMethod("as_epidist_naive_model")
}

#' The naive model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object
#' @method as_epidist_naive_model epidist_linelist_data
#' @family naive_model
#' @autoglobal
#' @export
as_epidist_naive_model.epidist_linelist_data <- function(data) {
  assert_epidist(data)

  data <- data |>
    mutate(delay = .data$stime_lwr - .data$ptime_lwr)

  data <- new_epidist_naive_model(data)
  assert_epidist(data)
  return(data)
}

#' Class constructor for `epidist_naive_model` objects
#'
#' @param data An object to be set with the class `epidist_naive_model`
#' @returns An object of class `epidist_naive_model`
#' @family naive_model
#' @export
new_epidist_naive_model <- function(data) {
  class(data) <- c("epidist_naive_model", class(data))
  return(data)
}

#' Check if data has the `epidist_naive_model` class
#'
#' @param data An object
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
