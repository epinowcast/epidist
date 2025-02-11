#' Transform data for an epidist model
#'
#' This function is used within [epidist()] to transform data before passing to
#' `brms`. It is unlikely that as a user you will need this function, but we
#' export it nonetheless to be transparent about what happens inside of a call
#' to [epidist()].
#'
#' @inheritParams epidist
#'
#' @param family A description of the response distribution and link function to
#'   be used in the model created using [epidist_family()].
#'
#' @param formula A formula object created using [epidist_formula()].
#'
#' @family transform_data
#' @export
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
#' @export
epidist_transform_data_model <- function(data, family, formula, ...) {
  UseMethod("epidist_transform_data_model")
}

#' Default method for transforming data for a model
#'
#' @inheritParams epidist_transform_data_model
#'
#' @family transform_data
#' @export
epidist_transform_data_model.default <- function(data, family, formula, ...) {
  return(data)
}
