test_that("epidist_stancode.default returns NULL", { # nolint: line_length_linter.
  expect_null(epidist_stancode(data.frame()))
})

# Collapse Stan source to a single line so that regexes can span lines
.flatten_stan <- function(x) {
  return(gsub("\\s+", " ", paste(x, collapse = " ")))
}

# Split a Stan argument list into bare argument names
.stan_arg_names <- function(x) {
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  return(sub(".*[[:space:]]", "", parts))
}

# Extract the argument list of a call or declaration by name
.stan_call_args <- function(source, pattern) {
  source <- .flatten_stan(source)
  matched <- regmatches(source, regexpr(pattern, source))
  expect_length(matched, 1)
  return(.stan_arg_names(sub(pattern, "\\1", matched)))
}

test_that("the marginal model passes delay_min as L and relative_obs_t as D to primarycensored_lpmf", { # nolint: line_length_linter.
  # The argument order of primarycensored_lpmf is read from the installed
  # primarycensored Stan source rather than hard coded, so a change upstream
  # is caught here rather than silently changing the likelihood.
  pcd_args <- .stan_call_args(
    primarycensored::pcd_load_stan_functions("primarycensored_lpmf"),
    "real primarycensored_lpmf\\((.*?)\\) \\{"
  )
  expect_true(all(c("L", "D") %in% pcd_args))

  # The epidist call site, read before any regex substitution is applied.
  chunk <- .stan_chunk(file.path("marginal_model", "functions.stan"))
  call_args <- .stan_call_args(
    gsub("|", ",", chunk, fixed = TRUE),
    "return primarycensored_lpmf\\((.*?)\\);"
  )

  expect_length(call_args, length(pcd_args))
  expect_identical(call_args[which(pcd_args == "L")], "delay_min")
  expect_identical(call_args[which(pcd_args == "D")], "relative_obs_t")
})

test_that("the marginal model Stan signature matches the vreal order in the formula", { # nolint: line_length_linter.
  # brms passes vreal1 ... vreal5 positionally, in the order given by
  # epidist_formula_model.epidist_marginal_model(). Check that the Stan
  # signature lines up with that order.
  formula <- epidist_formula_model(
    prep_marginal_obs,
    brms::bf(mu ~ 1, sigma ~ 1)
  )
  vreal_args <- .stan_call_args(
    deparse(formula$formula),
    "vreal\\((.*?)\\)"
  )
  expect_identical(
    vreal_args,
    c("relative_obs_time", "pwindow", "swindow", "delay_upr", "delay_min")
  )

  chunk <- .stan_chunk(file.path("marginal_model", "functions.stan"))
  stan_args <- .stan_call_args(
    chunk,
    "real marginal_family_lpmf\\((.*?)\\) \\{"
  )
  # y and dpars_A come first, primary_params last
  expect_identical(
    stan_args[3:7],
    c(
      "relative_obs_t", "pwindow_width", "swindow_width", "y_upper",
      "delay_min"
    )
  )
})
