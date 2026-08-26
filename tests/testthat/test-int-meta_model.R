# fmt: skip file
test_that("epidist.epidist_meta_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a gamma delay", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = Gamma(link = "log"),
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with summary estimates only", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  expect_s3_class(fit_meta_estimates, "brmsfit")
  expect_s3_class(fit_meta_estimates, "epidist_fit")
  expect_convergence(fit_meta_estimates)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from biased summary estimates", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_estimates)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.15)
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  expect_s3_class(fit_meta_mixed, "brmsfit")
  expect_s3_class(fit_meta_mixed, "epidist_fit")
  expect_convergence(fit_meta_mixed)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_mixed)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_meta_model log_lik and posterior_predict have the expected shapes", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  log_lik <- brms::log_lik(fit_meta_estimates)
  expect_identical(ncol(log_lik), nrow(prep_meta_biased))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_estimates)
  expect_identical(ncol(pred), nrow(prep_meta_biased))
  expect_true(all(is.finite(pred)))
})
