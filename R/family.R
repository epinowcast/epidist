#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This object is passed to `brms`. It is unlikely that
#' as a user you will need this function, but we export it nonetheless to be
#' transparent about what exactly is happening inside of a call to [epidist()].
#'
#' @param data A `data.frame` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param ... ...
#'
#' @family family
#' @export
epidist_family <- function(data, family = "lognormal", ...) {
  # allows use of stats::family and strings
  family <- brms:::validate_family(family)

  # other is all dpar but mu
  other_links <- family[[paste0("link_", setdiff(family$dpars, "mu"))]]
  other_bounds <- lapply(
    family$dpars[-1], brms:::dpar_bounds, family = family$family
  )
  family$other_links <- other_links
  family$other_bounds <- other_bounds

  custom_family <- epidist_family_model(data, family, ...)
  custom_family <- epidist_family_reparam(custom_family)

  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @inheritParams epidist_family
#' @param ... Additional arguments passed to method.
#' @rdname epidist_family_model
#' @family family
#' @export
epidist_family_model <- function(data, ...) {
  UseMethod("epidist_family_model")
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' @inheritParams epidist_family
#' @param ... Additional arguments passed to method.
#' @rdname epidist_family_family
#' @family family
#' @export
epidist_family_reparam <- function(family, ...) {
  UseMethod("epidist_family_family")
}

#' Default method for families which do not require a reparameterisation
#'
#' @inheritParams epidist_family_reparam
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_family_reparam.default <- function(family, ...) {
  if (family$family == "gamma") {
    family$reparam <- c("shape", "shape ./ mu")
  } else {
    family$reparam <- family$dpars
  }
  return(family)
}
