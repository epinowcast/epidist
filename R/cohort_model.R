#' Prepare cohort model
#'
#' @param data A `data.frame` containing line list data
#' @family cohort_model
#' @export
as_direct_model <- function(data) {
  UseMethod("as_cohort_model")
}

assert_cohort_model_input <- function(data) {
 # ...
}

#' Prepare cohort model
#'
#' @param data A `data.frame` containing line list data
#' @rdname as_direct_model
#' @method as_direct_model data.frame
#' @family cohort_model
#' @autoglobal
#' @export
as_direct_model.data.frame <- function(data) {
  assert_direct_model_input(data)
  class(data) <- c("epidist_direct_model", class(data))
  data <- data |>
    mutate(delay = .data$stime - .data$ptime)
  epidist_validate(data)
  return(data)
}

#' Validate cohort model data
#'
#' @param data A `data.frame` containing line list data
#' @param ... ...
#' @method epidist_validate epidist_cohort_model
#' @family cohort_model
#' @export
epidist_validate.epidist_cohort_model <- function(data, ...) {
  assert_true(is_cohort_model(data))
  assert_cohort_model_input(data)
}

#' Check if data has the `epidist_cohort_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family cohort_model
#' @export
is_direct_model <- function(data) {
  inherits(data, "epidist_direct_model")
}
