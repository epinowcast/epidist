#' Create custom family for `epidist` model
#'
#' @param data A `data.frame` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param ... ...
#'
#' @family family
#' @export
epidist_family <- function(data, family = "lognormal", ...) {
  # epidist_family_model
  # epidist_family_family
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @rdname epidist_family_family
#' @family family
#' @export
epidist_family_model <- function(family, ...) {
  UseMethod("epidist_family_family")
}

#' The family-specific parts of an `epidist_family()` call
#'
#' @rdname epidist_family_family
#' @family family
#' @export
epidist_family_family <- function(family, ...) {
  UseMethod("epidist_family_family")
}
