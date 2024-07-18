test_that("", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit_laplace <- epidist(data = prep_obs, seed = 1, algorithm = "laplace")
  diag <- epidist_diagnostics(fit_laplace)
})


test_that("", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  diag <- epidist_diagnostics(fit)
})