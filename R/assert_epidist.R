#' Validation for epidist objects
#'
#' @param data Object to validate
#' @param ... Additional arguments
#' @return NULL invisibly
#' @export
#' @family assert
assert_epidist <- function(data, ...) {
  UseMethod("assert_epidist")
}

#' @export
#' @family assert
assert_epidist.default <- function(data, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a valid epidist object.",
      "i" = "Please convert to epidist object first using as_epidist_<class>()" # nolint
    )
  )
}
