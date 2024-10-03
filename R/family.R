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
  custom_family <- epidist_family_model(data, family, ...)
  # epidist_family_family()
  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @rdname epidist_family_model
#' @family family
#' @export
epidist_family_model <- function(data, ...) {
  UseMethod("epidist_family_model")
}

#' The family-specific parts of an `epidist_family()` call
#'
#' @rdname epidist_family_family
#' @family family
#' @export
epidist_family_family <- function(family, ...) {
  UseMethod("epidist_family_family")
}
