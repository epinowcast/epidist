#' Prepare data for modelling
#'
#' @param data A dataframe to be used for modelling
#' @rdname prepare
#' @export
prepare <- function(data, ...) {
  UseMethod("prepare")
}

#' Default method used when preparing data
#'
#' @param model Character string, model type to prepare to use.
#' Supported options are "ltcad".
#' @param ... Additional arguments passed to model specific prepare functions
#' @rdname prepare
#' @method prepare default
#' @export
prepare.default <- function(data, model, ...) {
  model <- match.arg(model, choices = c("ltcad"))
  class(data) <- c(class(data), paste0("epidist_", model))
  prepare(data, ...)
}

#' Define model specific priors
#' 
#' @export
#' @inheritParams epidist
#' @param ... Additional arguments for method.
#' @rdname epidist_priors
epidist_priors <- function(data, ...) {
  UseMethod("epidist_priors")
}

#' Define model specific stancode
#' 
#' @export
#' @inheritParams epidist_priors
#' @param ... Additional arguments for method.
#' @rdname id_stancode
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Define a model specific formula
#' 
#' @export
#' @inheritParams epidist_priors
#' @param ... Additional arguments for method.
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Interface using `brms`
#'
#' @param data
#' @param formula
#' @param family
#' @param priors
#' @param custom_stancode
#' @param dry
#' @param ... Additional arguments for method.
#' @export
epidist <- function(data, formula, family, priors, custom_stancode, dry = FALSE,
                    ...) {
  UseMethod("epidist")
}