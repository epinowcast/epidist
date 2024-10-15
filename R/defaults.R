#' Default method for data validation
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_validate.default <- function(data, ...) {
  cli_abort(
    "No epidist_validate method implemented for the class ", class(data), "\n",
    "See methods(epidist_validate) for available methods"
  )
}

#' Default method for defining model specific Stan code
#'
#' @inheritParams epidist_stancode
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_stancode.default <- function(data, ...) {
  cli_abort(
    "No epidist_stancode method implemented for the class ", class(data), "\n",
    "See methods(epidist_stancode) for available methods"
  )
}
