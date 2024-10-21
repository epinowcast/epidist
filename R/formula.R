#' Define a model specific formula
#'
#' This function is used within [epidist()] to create the formula object passed
#' to `brms`. It is unlikely that as a user you will need this function, but we
#' export it nonetheless to be transparent about what exactly is happening
#' inside of a call to [epidist()].
#'
#' @inheritParams epidist
#' @param family A description of the response distribution and link function to
#' be used in the model created using [epidist_family()].
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
#' @inheritParams epidist
#' @rdname epidist_family_model
#' @family formula
#' @export
epidist_formula_model <- function(data, formula, ...) {
  UseMethod("epidist_formula_model")
}

#' Default method for defining a model specific formula
#'
#' @inheritParams epidist
#' @family formula
#' @export
epidist_formula_model.default <- function(data, formula, ...) {
  return(formula)
}
