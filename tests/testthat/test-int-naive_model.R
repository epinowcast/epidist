test_that("epidist.epidist_naive_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(
    data = prep_naive_obs,
    fn = brms::make_stancode
  )
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_naive_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  fit <- epidist(
    data = prep_naive_obs,
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2
  )
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})
