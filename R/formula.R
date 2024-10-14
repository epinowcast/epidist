#' Define a model specific formula
#'
#' This function is used within [epidist()] to create the formula object passed
#' to `brms`. It is unlikely that as a user you will need this function, but we
#' export it nonetheless to be transparent about what exactly is happening
#' inside of a call to [epidist()].
#'
#' @param data A `data.frame` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param formula As produced by [brms::brmsformula()]
#' @param ... ...
#'
#' @family formula
#' @export
epidist_formula <- function(data, family, formula, ...) {
  epidist_validate(data)
  formula <- brms:::validate_formula(formula, data = data, family = family)
  formula <- .make_intercepts_explicit(formula)
  formula <- epidist_formula_model(data, formula)
  # Using this here for checking purposes
  bterms <- brms::brmsterms(formula)
  brms:::validate_data(data, bterms)
  return(formula)
}

#' The model-specific parts of an `epidist_formula()` call
#'
#' @inheritParams epidist_formula
#' @param ... Additional arguments passed to method.
#' @rdname epidist_family_model
#' @family formula
#' @export
epidist_formula_model <- function(data, formula, ...) {
  UseMethod("epidist_formula_model")
}

#' Default method for defining a model specific formula
#'
#' @inheritParams epidist_formula
#' @param ... Additional arguments passed to method.
#' @family formula
#' @export
epidist_formula_model.default <- function(data, formula, ...) {
  return(formula)
}
