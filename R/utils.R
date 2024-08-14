#' Read in a epidist Stan code chunk
#'
#' @param path The path within the "stan" folder of the installed epidist
#' package to the Stan code chunk of interest.
#' @return A character string containing the Stan code chunk of interest.
#' @family utils
#' @export
epidist_stan_chunk <- function(path) {
  local_path <- system.file(paste0("stan/", path), package = "epidist")
  paste(readLines(local_path), collapse = "\n")
}

#' Label a epidist Stan model with a version indicator
#'
#' @return A brms stanvar chunk containing the package version used to build
#' the Stan code.
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
#' Or, if `f = 1` then `floor_mult` behaves as `floor`.
#'
#' @param x A number to be rounded down
#' @param f A number specifying the multiple to be rounded down to
#' @family utils
#' @export
floor_mult <- function(x, f = 1) {
  return(floor(x / f) * f)
}
