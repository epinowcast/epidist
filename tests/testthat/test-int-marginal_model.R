# fmt: skip file
test_that("epidist.epidist_marginal_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_marginal_obs,
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_marginal_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  expect_s3_class(fit_marginal, "brmsfit")
  expect_s3_class(fit_marginal, "epidist_fit")
  expect_convergence(fit_marginal)
})

test_that("epidist.epidist_marginal_model recovers the simulation settings for the delay distribution in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit_marginal)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_marginal_model fits and the MCMC converges in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  expect_s3_class(fit_marginal_gamma, "brmsfit")
  expect_s3_class(fit_marginal_gamma, "epidist_fit")
  expect_convergence(fit_marginal_gamma)
})

test_that("epidist.epidist_marginal_model recovers the simulation settings for the delay distribution in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  draws_gamma <- posterior::as_draws_df(fit_marginal_gamma$fit)
  draws_gamma_mu <- exp(draws_gamma$Intercept)
  draws_gamma_shape <- exp(draws_gamma$Intercept_shape)
  draws_gamma_mu_ecdf <- ecdf(draws_gamma_mu)
  draws_gamma_shape_ecdf <- ecdf(draws_gamma_shape)
  quantile_mu <- draws_gamma_mu_ecdf(mu)
  quantile_shape <- draws_gamma_shape_ecdf(shape)
  expect_gte(quantile_mu, 0.025)
  expect_lte(quantile_mu, 0.975)
  expect_gte(quantile_shape, 0.025)
  expect_lte(quantile_shape, 0.975)
})

test_that("epidist.epidist_marginal_model fits and recovers a sex effect", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  expect_s3_class(fit_marginal_sex, "brmsfit")
  expect_s3_class(fit_marginal_sex, "epidist_fit")
  expect_convergence(fit_marginal_sex)

  draws <- posterior::as_draws_df(fit_marginal_sex$fit)
  expect_equal(mean(draws$b_Intercept), meanlog_m, tolerance = 0.3)
  expect_equal(
    mean(draws$b_Intercept + draws$b_sex), meanlog_f,
    tolerance = 0.3
  )
  expect_equal(mean(exp(draws$b_sigma_Intercept)), sdlog_m, tolerance = 0.3)
  expect_equal(
    mean(exp(draws$b_sigma_Intercept + draws$b_sigma_sex)),
    sdlog_f,
    tolerance = 0.3
  )
})
