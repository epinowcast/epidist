#' Transform data for an epidist model
#'
#' This function is used within [epidist()] to transform data before passing to
#' `brms`. It dispatches to [epidist_transform_data_model()], which is the
#' generic a custom model implements. This wrapper is internal; extensions
#' should write a method for [epidist_transform_data_model()] instead.
#'
#' @inheritParams epidist
#'
#' @param family A description of the response distribution and link function to
#'   be used in the model created using [epidist_family()].
#'
#' @param formula A formula object created using [epidist_formula()].
#'
#' @family transform_data
#' @keywords internal
epidist_transform_data <- function(data, family, formula, ...) {
  assert_epidist(data)
  data <- epidist_transform_data_model(data, family, formula)
  return(data)
}

#' The model-specific parts of an `epidist_transform_data()` call
#'
#' @inheritParams epidist_transform_data
#'
#' @rdname epidist_transform_data_model
#'
#' @family transform_data
#' @returns The data transformed ready for fitting.
#'
#' @export
epidist_transform_data_model <- function(data, family, formula, ...) {
  UseMethod("epidist_transform_data_model")
}

#' Default method for transforming data for a model
#'
#' @inheritParams epidist_transform_data_model
#'
#' @family transform_data
#' @returns The data transformed ready for fitting.
#'
#' @export
epidist_transform_data_model.default <- function(data, family, formula, ...) {
  return(data)
}
