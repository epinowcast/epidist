# Note: some tests in this script are stochastic. As such, test failure may be
# bad luck rather than indicate an issue with the code. However, as these tests
# are reproducible, the distribution of test failures may be investigated by
# varying the input seed. Test failure at an unusually high rate does suggest
# a potential code issue.

test_that("epidist.epidist_latent_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(
    data = prep_obs,
    fn = brms::make_stancode
  )
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_latent_model samples from the prior according to marginal Kolmogorov-Smirnov tests in the default case.", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  prior_samples <- epidist(
    data = prep_obs,
    fn = brms::brm,
    sample_prior = "only",
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2
  )
  pred <- predict_delay_parameters(prior_samples)
  family <- lognormal()
  epidist_family <- epidist_family(data = prep_obs, family = family)
  epidist_formula <- epidist_formula(
    data = prep_obs,
    family = epidist_family,
    formula = mu ~ 1
  )
  epidist_prior <- epidist_prior(
    data = prep_obs,
    family = epidist_family,
    formula = epidist_formula,
    prior = NULL
  )
  param1 <- extract_normal_parameters_brms(epidist_prior[1, ])
  param2 <- extract_normal_parameters_brms(epidist_prior[2, ])
  samples1 <- rnorm(1000, mean = param1$mean, sd = param1$sd)
  samples2 <- exp(rnorm(1000, mean = param2$mean, sd = param2$sd))
  # suppressWarnings here used to prevent warnings about ties
  ks1 <- suppressWarnings(stats::ks.test(pred$mu, samples1))
  ks2 <- suppressWarnings(stats::ks.test(pred$sigma, samples2))
  testthat::expect_gt(ks1$p.value, 0.01)
  testthat::expect_gt(ks2$p.value, 0.01)
})

test_that("epidist.epidist_latent_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_latent_model fits, the MCMC converges, and the draws of sigma are indeed a constant, when setting sigma = 1 (a constant)", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  fit_constant <- epidist(
    data = prep_obs,
    formula = bf(mu ~ 1, sigma = 1),
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2
  )
  expect_s3_class(fit_constant, "brmsfit")
  expect_s3_class(fit_constant, "epidist_fit")
  expect_convergence(fit_constant)
  sigma <- rstan::extract(fit_constant$fit, pars = "sigma")$sigma
  expect_true(all(sigma == 1))
})

test_that("epidist.epidist_latent_model Stan code has no syntax errors", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  stancode_string <- epidist(
    data = prep_obs,
    family = lognormal(),
    seed = 1,
    silent = 2, refresh = 0,
    fn = brms::make_stancode
  )
  mod_string <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode_string), compile = FALSE
  )
  expect_true(mod_string$check_syntax())
})

test_that("epidist.epidist_latent_model recovers the simulation settings for the delay distribution in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit)
  # Unclear the extent to which we should expect parameter recovery here
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_latent_model Stan code has no syntax errors and compiles in the gamma delay case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode_gamma <- epidist(
    data = prep_obs_gamma,
    family = Gamma(link = "log"),
    formula = mu ~ 1,
    cores = 2,
    fn = brms::make_stancode
  )
  mod_gamma <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode_gamma), compile = FALSE
  )
  expect_true(mod_gamma$check_syntax())
  expect_no_error(mod_gamma$compile())
})

test_that("epidist.epidist_latent_model fits and the MCMC converges in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  expect_s3_class(fit_gamma, "brmsfit")
  expect_s3_class(fit_gamma, "epidist_fit")
  expect_convergence(fit_gamma)
})

test_that("epidist.epidist_latent_model recovers the simulation settings for the delay distribution in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  draws_gamma <- posterior::as_draws_df(fit_gamma$fit)
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

test_that("epidist.epidist_latent_model Stan code has no syntax errors for an alternative formula", { # nolint: line_length_linter.
  skip_on_cran()
  stancode_sex <- epidist(
    data = prep_obs_sex,
    formula = bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    fn = brms::make_stancode,
    cores = 2
  )
  mod_sex <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode_sex), compile = FALSE
  )
  expect_true(mod_sex$check_syntax())
})

test_that("epidist.epidist_latent_model recovers a sex effect", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  draws <- posterior::as_draws_df(fit_sex$fit)
  expect_equal(mean(draws$b_Intercept), meanlog_m, tolerance = 0.2)
  expect_equal(
    mean(draws$b_Intercept + draws$b_sex), meanlog_f,
    tolerance = 0.2
  )
  expect_equal(mean(exp(draws$b_sigma_Intercept)), sdlog_m, tolerance = 0.2)
  expect_equal(
    mean(exp(draws$b_sigma_Intercept + draws$b_sigma_sex)),
    sdlog_f,
    tolerance = 0.2
  )
  expect_s3_class(fit_sex, "brmsfit")
  expect_s3_class(fit_sex, "epidist_fit")
  expect_convergence(fit_sex)
})
