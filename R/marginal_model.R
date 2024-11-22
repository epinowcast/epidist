#' Prepare marginal model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @family marginal_model
#' @export
as_epidist_marginal_model <- function(data) {
  UseMethod("as_epidist_marginal_model")
}

#' The marginal model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object
#' @method as_epidist_marginal_model epidist_linelist_data
#' @family marginal_model
#' @autoglobal
#' @export
as_epidist_marginal_model.epidist_linelist_data <- function(data) {
  assert_epidist(data)

  # Here we do the processing to turn an epidist_linelist_data into an aggregate
  # dataset. In the future this would be refactored into a function which
  # converts from linelist data to aggregate data, and a function which goes
  # from aggregate data into the marginal model class
  data <- data |>
    mutate(delay = .data$stime_lwr - .data$ptime_lwr) |>
    dplyr::group_by(delay) |>
    dplyr::summarise(count = dplyr::n())

  data <- new_epidist_marginal_model(data)
  assert_epidist(data)
  return(data)
}

#' Class constructor for `epidist_marginal_model` objects
#'
#' @param data A data.frame to convert
#' @returns An object of class `epidist_marginal_model`
#' @family marginal_model
#' @export
new_epidist_marginal_model <- function(data) {
  class(data) <- c("epidist_marginal_model", class(data))
  return(data)
}

#' @method assert_epidist epidist_marginal_model
#' @family marginal_model
#' @export
assert_epidist.epidist_marginal_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = "delay")
  assert_numeric(data$delay, lower = 0)
}

#' Check if data has the `epidist_marginal_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family marginal_model
#' @export
is_epidist_marginal_model <- function(data) {
  inherits(data, "epidist_marginal_model")
}
