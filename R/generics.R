#' Validate
#'
#' @param data A dataframe to be used for modelling.
#' @family generics
#' @export
epidist_validate <- function(data) {
  UseMethod("epidist_validate")
}

#' Define a model specific formula
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Define model specific family
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist_family <- function(data, ...) {
  UseMethod("epidist_family")
}

#' Define model specific priors
#'
#' @inheritParams epidist_validate
#' @param family ...
#' @param formula ...
#' @param ... Additional arguments for method.
#' @rdname epidist_prior
#' @family generics
#' @export
epidist_prior <- function(data, family, formula, ...) {
  UseMethod("epidist_prior")
}

#' Define model specific Stan code
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments for method.
#' @rdname epidist_stancode
#' @family generics
#' @export
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Interface using `brms`
#'
#' @inheritParams epidist_validate
#' @param formula ...
#' @param family ...
#' @param fn Likely `brms::brm`. Also possible to be `brms::make_stancode` or
#' `brms::make_standata`.
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist <- function(data, formula, family, fn, ...) {
  UseMethod("epidist")
}
