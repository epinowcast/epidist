#' Prepare data for modelling
#'
#' @param data A dataframe to be used for modelling.
#' @rdname epidist_prepare
#' @family generics
#' @export
epidist_prepare <- function(data, ...) {
  UseMethod("epidist_prepare")
}

#' Define a model specific formula
#'
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Define model specific family
#'
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist_family <- function(data, ...) {
  UseMethod("epidist_family")
}

#' Define model specific priors
#'
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @rdname epidist_priors
#' @family generics
#' @export
epidist_priors <- function(data, ...) {
  UseMethod("epidist_priors")
}

#' Define model specific Stan code
#'
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @rdname epidist_stancode
#' @family generics
#' @export
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Interface using `brms`
#'
#' @param formula A formula as defined using [epidist_formula()]
#' @param family ...
#' @param priors ...
#' @param stancode ...
#' @param fn Likely `brms::brm`. Also possible to be `brms::make_stancode` or
#' `brms::make_standata`.
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist <- function(data, formula, family, priors, stancode, fn, ...) {
  UseMethod("epidist")
}
