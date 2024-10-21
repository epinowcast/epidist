#' Define model specific Stan code
#'
#' This function is used within [epidist()] to create any custom Stan code which
#' is injected into `brms` via the `stanvars` argument. It is unlikely that
#' as a user you will need this function, but we export it nonetheless to be
#' transparent about what exactly is happening inside of a call to [epidist()].
#'
#' @inheritParams epidist
#' @rdname epidist_stancode
#' @family stan
#' @export
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Default method for defining model specific Stan code
#'
#' @inheritParams epidist
#' @family stan
#' @export
epidist_stancode.default <- function(data, ...) {
  return(NULL)
}
