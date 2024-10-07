#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This custom family is passed to `brms`. It is unlikely
#' that as a user you will need this function, but we export it nonetheless to
#' be transparent about what happens inside of a call to [epidist()].
#'
#' @param data A `data.frame` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param ... ...
#'
#' @family family
#' @export
epidist_family <- function(data, family = "lognormal", ...) {
  epidist_validate(data)
  family <- brms:::validate_family(family)
  class(family) <- c(family$family, class(family))
  family <- .add_dpar_info(family)
  custom_family <- epidist_family_model(data, family, ...)
  class(custom_family) <- c(family$family, class(custom_family))
  custom_family <- epidist_family_reparam(custom_family)
  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @inheritParams epidist_family
#' @param family Output of a call to `brms::brmsfamily()` with additional
#' information as provided by `.add_dpar_info()`
#' @param ... Additional arguments passed to method.
#' @rdname epidist_family_model
#' @family family
#' @export
epidist_family_model <- function(data, family, ...) {
  UseMethod("epidist_family_model")
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family_model
#' @param ... Additional arguments passed to method.
#' @family family
#' @export
epidist_family_model.default <- function(data, ...) {
  cli_abort(
    "No epidist_family_model method implemented for the class ", class(data),
    "\n", "See methods(epidist_family_model) for available methods"
  )
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' @inheritParams epidist_family
#' @param ... Additional arguments passed to method.
#' @rdname epidist_family_reparam
#' @family family
#' @export
epidist_family_reparam <- function(family, ...) {
  UseMethod("epidist_family_reparam")
}

#' Default method for families which do not require a reparameterisation
#'
#' @inheritParams epidist_family_reparam
#' @param ... Additional arguments passed to method.
#' @family family
#' @export
epidist_family_reparam.default <- function(family, ...) {
  family$reparam <- family$dpars
  return(family)
}

#' Reparameterisation for the gamma family
#'
#' @inheritParams epidist_family_reparam
#' @param ... Additional arguments passed to method.
#' @family family
#' @export
epidist_family_reparam.gamma <- function(family, ...) {
  family$reparam <- c("shape", "shape ./ mu")
  return(family)
}
