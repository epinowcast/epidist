# Note: some tests in this script are stochastic. As such, test failure may be
# bad luck rather than indicate an issue with the code. However, as these tests
# are reproducible, the distribution of test failures may be investigated by
# varying the input seed. Test failure at an unusually high rate does suggest
# a potential code issue.

test_that("epidist.epidist_naive_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(
    data = prep_naive_obs,
    fn = brms::make_stancode
  )
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_naive_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
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
