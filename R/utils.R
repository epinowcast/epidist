#' Read in a epidist Stan code chunk
#'
#' @param path The path within the "stan" folder of the installed epidist
#' package to the Stan code chunk of interest.
#' @return A character string containing the Stan code chunk of interest.
#' @export
epidist_stan_chunk <- function(path) {
  local_path <- system.file(paste0("stan/", path), package = "epidist")
  paste(readLines(local_path), collapse = "\n")
}
