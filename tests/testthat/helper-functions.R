on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

not_on_cran <- function() {
  identical(Sys.getenv("NOT_CRAN"), "true")
}

skip_on_local <- function() {
  if (on_ci()) {
    return(invisible(TRUE))
  }
  testthat::skip("Not on CI")
}

as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

extract_normal_parameters_brms <- function(prior) {
  pattern <- "normal\\(([^,]+), ([^\\)]+)\\)"
  match <- regmatches(prior, regexec(pattern, prior))
  mean <- as.numeric(match[[1]][2])
  sd <- as.numeric(match[[1]][3])
  return(list(mean = mean, sd = sd))
}
