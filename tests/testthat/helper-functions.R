on_ci <- function() {
  return(isTRUE(as.logical(Sys.getenv("CI"))))
}

not_on_cran <- function() {
  return(identical(Sys.getenv("NOT_CRAN"), "true"))
}

has_cmdstanr <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    return(FALSE)
  }
  installed <- try(
    cmdstanr::cmdstan_version(error_on_NA = FALSE),
    silent = TRUE
  )
  return(!inherits(installed, "try-error") && !is.null(installed))
}

skip_if_no_cmdstanr <- function() {
  if (has_cmdstanr()) {
    return(invisible(TRUE))
  }
  return(testthat::skip("cmdstanr or CmdStan is not available"))
}

skip_on_local <- function() {
  if (on_ci()) {
    return(invisible(TRUE))
  }
  return(testthat::skip("Not on CI"))
}

as_string_formula <- function(formula) {
  form <- deparse1(formula, collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

extract_normal_parameters_brms <- function(prior) {
  pattern <- "normal\\(([^,]+), ([^\\)]+)\\)" # nolint
  matched <- regmatches(prior, regexec(pattern, prior))
  prior_mean <- as.numeric(matched[[1]][2])
  prior_sd <- as.numeric(matched[[1]][3])
  return(list(mean = prior_mean, sd = prior_sd))
}
