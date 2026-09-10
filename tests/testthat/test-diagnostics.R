# fmt: skip file
test_that("epidist_diagnostics", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  diag <- epidist_diagnostics(fit)
  expected_names <- c(
    "time", "samples", "max_rhat", "divergent_transitions",
    "per_divergent_transitions", "max_treedepth", "no_at_max_treedepth",
    "per_at_max_treedepth"
  )
  expect_named(diag, expected_names)
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

test_that("epidist_diagnostics gives an error when passed a model fit with an approximate algorithm", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  prep_obs <- as_epidist_latent_model(sim_obs)
  # The variational fit only has to exist, so its Pareto k warning is noise.
  fit_meanfield <- suppressWarnings(epidist(
    data = prep_obs, seed = 1, algorithm = "meanfield",
    refresh = 0, silent = 2
  ))
  expect_error(epidist_diagnostics(fit_meanfield))
})
