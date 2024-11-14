test_that("epidist_diagnostics", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  diag <- epidist_diagnostics(fit)
  expected_names <- c(
    "time", "samples", "max_rhat", "divergent_transitions",
    "per_divergent_transitions", "max_treedepth", "no_at_max_treedepth",
    "per_at_max_treedepth"
  )
  expect_equal(names(diag), expected_names)
  expect_gt(diag$time, 0)
  expect_gt(diag$samples, 0)
  expect_gt(diag$max_rhat, 0.9)
  expect_lt(diag$max_rhat, 1.1)
  expect_gte(diag$divergent_transitions, 0)
  expect_lt(diag$divergent_transitions, diag$samples)
  expect_lt(diag$max_treedepth, 12)
  expect_lte(diag$no_at_max_treedepth, diag$samples)
  expect_lte(diag$per_at_max_treedepth, 1)
  expect_gt(diag$per_at_max_treedepth, 0)
})

test_that("epidist_diagnostics gives the same results for cmdstanr and rstan", {
  skip_on_cran()
  set.seed(1)
  diag_cmdstanr <- epidist_diagnostics(fit)
  diag_rstan <- epidist_diagnostics(fit_rstan)
  expect_equal(colnames(diag_cmdstanr), colnames(diag_rstan))
  expect_gt(diag_rstan$time, 0)
  expect_gt(diag_rstan$samples, 0)
  expect_gt(diag_rstan$max_rhat, 0.9)
  expect_lt(diag_rstan$max_rhat, 1.1)
  expect_gte(diag_rstan$divergent_transitions, 0)
  expect_lt(diag_rstan$divergent_transitions, diag_rstan$samples)
  expect_lt(diag_rstan$max_treedepth, 12)
  expect_lte(diag_rstan$no_at_max_treedepth, diag_rstan$samples)
  expect_lte(diag_rstan$per_at_max_treedepth, 1)
  expect_gt(diag_rstan$per_at_max_treedepth, 0)
})

test_that("epidist_diagnostics gives an error when passed model fit using the Laplace algorithm", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_model(sim_obs)
  fit_laplace <- epidist(
    data = prep_obs, seed = 1, algorithm = "laplace", backend = "cmdstanr",
    refresh = 0, silent = 2, show_messages = FALSE
  )
  expect_error(epidist_diagnostics(fit_laplace))
})
