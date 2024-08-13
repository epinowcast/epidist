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

#' Replace a brms prior only if it exists
#' 
#' @param old_prior One or more prior distributions in the class `brmsprior`
#' @param new_prior One prior distribution in the class `brmsprior`
#' @family utils
#' @export
replace_brms_prior <- function(old_prior, new_prior) {
  cols <- c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub")
  row_matches <- apply(old_prior[cols], 1, function(r) {
    all(r == as.vector(new_prior)[cols])
  })
  print(row_matches)
  if (any(row_matches)) {
    old <- capture.output(print(old_prior[row_matches, ]))
    old <- paste(old, collapse = "\n")
    new <- capture.output(print(new_prior))
    new <- paste(new, collapse = "\n")
    msg <- c(
      "i" = "Overwriting the brms prior:", old,
      "Using the prior:", new
    )
    cli::cli_inform(
      message = msg
    ) 
    # .frequency = "regularly", .frequency_id = "prior-message"
    old_prior[row_matches, ] <- new_prior
  }
  return(old_prior)
}
