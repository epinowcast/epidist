#' Validate data class
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate_data <- function(data, ...) {
  UseMethod("epidist_validate_data")
}

#' Default method for validate data class
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate_data.default <- function(data, ...) {
  cli_abort(
    "No epidist_validate_data method implemented for the class ", class(data),
    "\n", "See methods(epidist_validate_data) for available methods"
  )
}

#' Validate model class
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate_model <- function(data, ...) {
  UseMethod("epidist_validate_model")
}

#' Default method for validate model class
#'
#' @inheritParams epidist
#' @family validate
#' @export
epidist_validate_model.default <- function(data, ...) {
  cli_abort(
    "No epidist_validate_model method implemented for the class ", class(data),
    "\n", "See methods(epidist_validate_model) for available methods"
  )
}
