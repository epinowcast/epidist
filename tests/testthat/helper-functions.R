on_ci <- function() {
  return(isTRUE(as.logical(Sys.getenv("CI"))))
}

not_on_cran <- function() {
  return(identical(Sys.getenv("NOT_CRAN"), "true"))
}

# `fits_available` is set in setup.R, which builds the shared model fits.
skip_if_no_fits <- function() {
  if (exists("fits_available") && isTRUE(fits_available)) {
    return(invisible(TRUE))
  }
  return(testthat::skip("the shared model fits were not built"))
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

# Study labels for the lockstep fixtures of setup.R. The fixtures are bound
# into one object, so a label used by two of them merges two distinct studies
# into one and quietly changes what the lockstep tests compare. Labels are
# namespaced by the fixture that owns them, which is why the letters passed
# here are local to a fixture and need not continue any sequence.
lockstep_studies <- function(fixture, labels) {
  return(paste(fixture, labels, sep = "_"))
}

# Check the lockstep fixtures before they are bound. Each must be named, must
# carry only labels from its own namespace, and must share no label with
# another, so that a fixture added on a branch fails here rather than merging
# cleanly into a silent collision.
check_lockstep_studies <- function(fixtures) {
  tags <- names(fixtures)
  if (is.null(tags) || !all(nzchar(tags))) {
    return(cli::cli_abort("Every lockstep fixture must be named."))
  }
  studies <- lapply(fixtures, function(fixture) {
    return(unique(as.character(fixture$study)))
  })
  foreign <- unlist(
    Map(function(name, study) {
      return(study[!startsWith(study, paste0(name, "_"))])
    }, tags, studies),
    use.names = FALSE
  )
  if (length(foreign) > 0) {
    return(cli::cli_abort(c(
      "Every lockstep study label must name the fixture it belongs to.",
      x = "Outside its own fixture: {.val {foreign}}."
    )))
  }
  used <- unlist(studies, use.names = FALSE)
  shared <- unique(used[duplicated(used)])
  if (length(shared) > 0) {
    return(cli::cli_abort(c(
      "Every lockstep study label must be used by one fixture only.",
      x = "Shared between fixtures: {.val {shared}}."
    )))
  }
  return(unname(fixtures))
}
