#' Prepare direct model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @family direct_model
#' @export
as_direct_model <- function(data) {
  UseMethod("as_direct_model")
}

#' The direct model method for `epidist_linelist` objects
#'
#' @param data An `epidist_linelist` object
#' @method as_direct_model epidist_linelist
#' @family direct_model
#' @autoglobal
#' @export
as_direct_model.epidist_linelist <- function(data) {
  assert_epidist(data)

  data <- data |>
    mutate(delay = .data$stime_lwr - .data$ptime_lwr)

  data <- new_epidist_direct_model(data)
  assert_epidist(data)
  return(data)
}

#' Class constructor for `epidist_direct_model` objects
#'
#' @param data A data.frame to convert
#' @returns An object of class `epidist_direct_model`
#' @family direct_model
#' @export
new_epidist_direct_model <- function(data) {
  class(data) <- c("epidist_direct_model", class(data))
  return(data)
}

#' @method assert_epidist epidist_direct_model
#' @family direct_model
#' @export
assert_epidist.epidist_direct_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = c("delay"))
  assert_numeric(data$delay, lower = 0)
}

#' Check if data has the `epidist_direct_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family direct_model
#' @export
is_direct_model <- function(data) {
  inherits(data, "epidist_direct_model")
}
