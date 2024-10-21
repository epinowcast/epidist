#' Validate a data object for use with [epidist()]
#'
#' This function validates that the provided `data` is suitable to run a
#' particular `epidist` model. This may include checking the class of `data`,
#' and that it contains suitable columns.
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate <- function(data, ...) {
  UseMethod("epidist_validate")
}

#' Default method for data validation
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate.default <- function(data, ...) {
  cli_abort(
    "No epidist_validate method implemented for the class ", class(data), "\n",
    "See methods(epidist_validate) for available methods"
  )
}
