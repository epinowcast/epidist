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