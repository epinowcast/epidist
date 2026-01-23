on_ci <- function() {
  return(isTRUE(as.logical(Sys.getenv("CI"))))
}

not_on_cran <- function() {
  return(identical(Sys.getenv("NOT_CRAN"), "true"))
}

skip_on_local <- function() {
  if (on_ci()) {
    return(invisible(TRUE))
  }
  return(testthat::skip("Not on CI"))
}

as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

extract_normal_parameters_brms <- function(prior) {
  pattern <- "normal\\(([^,]+), ([^\\)]+)\\)" # nolint
  matches <- regmatches(prior, regexec(pattern, prior))
  mean_val <- as.numeric(matches[[1]][2])
  sd_val <- as.numeric(matches[[1]][3])
  return(list(mean = mean_val, sd = sd_val))
}
