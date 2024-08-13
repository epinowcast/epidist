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
#' @importFrom cli cli_inform
#' @autoglobal
#' @export
replace_brms_prior <- function(old_prior, new_prior) {
  if (is.null(new_prior)) {
    return(old_prior)
  }
  cols <- c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub")
  prior <- dplyr::full_join(
    old_prior, new_prior, by = cols, suffix = c("_old", "_new")
  )

  if (any(is.na(prior$prior_old))) {
    missing_prior <- capture.output(print(
      prior |>
        dplyr::filter(is.na(prior_old)) |>
        dplyr::select(prior = prior_new, all_of(cols), source = source_new)
    ))
    msg <- c(
      "i" = "No available prior to replace in old_prior found for:",
      missing_prior
    )
    cli::cli_abort(message = msg)
  }

  prior <- prior |>
    dplyr::filter(!is.na(prior_old), !is.na(prior_new)) |>
    dplyr::select(prior = prior_new, all_of(cols), source = source_new)

  return(prior)
}
