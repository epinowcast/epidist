#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This custom family is passed to `brms`. It is unlikely
#' that as a user you will need this function, but we export it nonetheless to
#' be transparent about what happens inside of a call to [epidist()].
#'
#' @inheritParams epidist
#' @family family
#' @export
epidist_family <- function(data, family = "lognormal", ...) {
  epidist_validate_model(data)
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
#' @inheritParams epidist
#' @param family Output of a call to `brms::brmsfamily()` with additional
#' information as provided by `.add_dpar_info()`
#' @rdname epidist_family_model
#' @family family
#' @export
epidist_family_model <- function(data, family, ...) {
  UseMethod("epidist_family_model")
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family_model
#' @family family
#' @export
epidist_family_model.default <- function(data, family, ...) {
  return(family)
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' @inheritParams epidist_family
#' @rdname epidist_family_reparam
#' @family family
#' @export
epidist_family_reparam <- function(family, ...) {
  UseMethod("epidist_family_reparam")
}

#' Default method for families which do not require a reparameterisation
#'
#' @inheritParams epidist_family
#' @family family
#' @export
epidist_family_reparam.default <- function(family, ...) {
  family$reparam <- family$dpars
  return(family)
}

#' Reparameterisation for the gamma family
#'
#' @inheritParams epidist_family
#' @family family
#' @export
epidist_family_reparam.gamma <- function(family, ...) {
  family$reparam <- c("shape", "shape ./ mu")
  return(family)
}
