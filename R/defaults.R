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
