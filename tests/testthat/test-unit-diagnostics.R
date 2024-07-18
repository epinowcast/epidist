test_that("epidist_diagnostics", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  diag <- epidist_diagnostics(fit)
  expected_names <- c(
    "time", "samples", "max_rhat", "divergent_transitions",
    "per_divergent_transitions", "max_treedepth", "no_at_max_treedepth",
    "per_at_max_treedepth"
  )
  expect_equal(names(diag), expected_names)
})

test_that("epidist_diagnostics gives an error when passed model fit using the Laplace algorithm", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit_laplace <- epidist(data = prep_obs, seed = 1, algorithm = "laplace")
  expect_error(epidist_diagnostics(fit_laplace))
})
