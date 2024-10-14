#' Define a model specific formula
#'
#' This function is used within [epidist()] to create the formula object passed
#' to `brms`. It is unlikely that as a user you will need this function, but we
#' export it nonetheless to be transparent about what exactly is happening
#' inside of a call to [epidist()].
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @family generics
#' @export
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Default method for defining a model specific formula
#'
#' @inheritParams epidist_formula
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_formula.default <- function(data, ...) {
  cli_abort(
    "No epidist_formula method implemented for the class ", class(data), "\n",
    "See methods(epidist_formula) for available methods"
  )
}
