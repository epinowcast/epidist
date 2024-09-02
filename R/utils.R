#' Read in an `epidist` Stan code chunk
#'
#' This function is used to obtain Stan code chunks from the `stan/` folder of
#' the `epidist` package. It is used within the [epidist_stancode()] function.
#'
#' @param path The path within the `stan/` folder of the installed `epidist`
#' package to the Stan code chunk of interest.
#' @return A character string containing the Stan code chunk of interest.
#' @family utils
#' @export
epidist_stan_chunk <- function(path) {
  local_path <- system.file(paste0("stan/", path), package = "epidist")
  paste(readLines(local_path), collapse = "\n")
}

#' Label a `epidist` Stan model with a version indicator
#'
#' This function is used within [epidist_stancode()] to label the generated Stan
#' code with the version of `epidist` used. To view the full Stan code for any
#' particular `epidist` model, we recommend use of [brms::make_stancode()].
#'
#' @return A `brms` Stan chunk containing the `epidist` package version used to
#' build the Stan code.
#' @family utils
#' @export
epidist_version_stanvar <- function() {
  version <- utils::packageVersion("epidist")
  comment <- paste0("// code chunks used from epidist ", version, "\n")
  brms::stanvar(scode = comment, block = "functions")
}

#' Round to the nearest multiple
#'
#' This function rounds an input `x` down to the nearest multiple of some number
#' `f`. For example, if `f = 0.2` and `x = 1.5` then the output would be 1.4.
#' If `f = 1` then `floor_mult` behaves as `floor`. If `f = 0` then `x` is
#' returned.
#'
#' @param x A number to be rounded down
#' @param f A positive number specifying the multiple to be rounded down to
#' @importFrom checkmate assert_numeric
#' @family utils
floor_mult <- function(x, f = 1) {
  checkmate::assert_numeric(f, lower = 0)
  ifelse(f == 0, x, floor(x / f) * f)
}
