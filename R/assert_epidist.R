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

#' @returns `NULL`, invisibly. Called for the side effect of validating `data`.
#'
#' @export
#' @family assert
assert_epidist.default <- function(data, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a valid epidist object.",
      "i" = "Please convert to epidist object first using as_epidist_<class>()" # nolint
    )
  )
  return(invisible(NULL))
}

#' Reject a grouped `data.frame`
#'
#' The `as_epidist_*()` constructors that build a model or aggregate object
#' from `epidist_linelist_data` number rows or count observations in ways
#' that go wrong silently on a grouped `data.frame`, for example
#' [dplyr::row_number()] restarting within each group. Grouped input is
#' rejected here rather than accepted and mishandled.
#'
#' @param data An object to check.
#'
#' @returns `NULL`, invisibly. Called for the side effect of raising an error
#'  when `data` is a grouped `data.frame`.
#'
#' @keywords internal
#' @importFrom dplyr is_grouped_df
.assert_ungrouped <- function(data) {
  if (is_grouped_df(data)) {
    cli_abort(c(
      "!" = "{.arg data} must not be grouped.",
      "i" = "Call {.fn dplyr::ungroup} on it first." # nolint
    ))
  }
  return(invisible(NULL))
}
