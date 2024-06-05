#' Prepare data for modelling
#'
#' @param data A dataframe to be used for modelling.
#' @rdname epidist_prepare
#' @family methods
#' @export
epidist_prepare <- function(data, ...) {
  UseMethod("epidist_prepare")
}

#' Default method used when preparing data
#'
#' @param model Character string, model type to prepare to use.
#' Supported options are "ltcad".
#' @param ... Additional arguments passed to model specific `epidist_prepare`
#' functions
#' @rdname epidist_prepare
#' @method epidist_prepare default
#' @family methods
#' @export
epidist_prepare.default <- function(data, model, ...) {
  model <- match.arg(model, choices = c("ltcad"))
  class(data) <- c(class(data), paste0("epidist_", model))
  epidist_prepare(data, ...)
}

#' Define a model specific formula
#' 
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family methods
#' @export
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Define model specific family
#' 
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family methods
#' @export
epidist_family <- function(data, ...) {
  UseMethod("epidist_family")
}

#' Define model specific priors
#' 
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @rdname epidist_priors
#' @family methods
#' @export
epidist_priors <- function(data, ...) {
  UseMethod("epidist_priors")
}

#' Define model specific stancode
#' 
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @rdname id_stancode
#' @family methods
#' @export
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Interface using `brms`
#'
#' @param formula A formula as defined using [epidist_formula()]
#' @param family ...
#' @param priors ...
#' @param custom_stancode ...
#' @param fn Likely `brms::brm`. Also possible to be `brms::make_stancode` or
#' `brms::make_standata`.
#' @inheritParams epidist_prepare
#' @param ... Additional arguments for method.
#' @family methods
#' @export
epidist <- function(data, formula, family, priors, custom_stancode, fn, ...) {
  UseMethod("epidist")
}
