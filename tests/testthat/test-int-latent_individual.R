# Note: some tests in this script are stochastic. As such, test failure may be
# bad luck rather than indicate an issue with the code. However, as these tests
# are reproducible, the distribution of test failures may be investigated by
# varying the input seed. Test failure at an unusually high rate does suggest
# a potential code issue.

prep_obs <- as_latent_individual(sim_obs)
prep_obs_gamma <- as_latent_individual(sim_obs_gamma)

test_that("epidist.epidist_latent_individual Stan code has no syntax errors and compiles in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(data = prep_obs, fn = brms::make_stancode)
  mod <- cmdstan_model(stan_file = write_stan_file(stancode), compile = FALSE)
  expect_true(mod$check_syntax())
  expect_no_error(mod$compile())
})

extract_normal_parameters_brms <- function(prior) {
  pattern <- "normal\\(([^,]+), ([^\\)]+)\\)"
  match <- regmatches(prior, regexec(pattern, prior))
  mean <- as.numeric(match[[1]][2])
  sd <- as.numeric(match[[1]][3])
  return(list(mean = mean, sd = sd))
}

test_that("epidist.epidist_latent_individual samples from the prior according to marginal Kolmogorov-Smirnov tests in the default case.", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  prior_samples <- epidist(data = prep_obs, fn = brms::brm,
                           sample_prior = "only", seed = 1)
  lognormal_draws <- extract_lognormal_draws(prior_samples)
  prior <- epidist_prior(data = prep_obs)
  param1 <- extract_normal_parameters_brms(prior[1, ])
  param2 <- extract_normal_parameters_brms(prior[2, ])
  samples1 <- rnorm(1000, mean = param1$mean, sd = param1$sd)
  samples2 <- exp(rnorm(1000, mean = param2$mean, sd = param2$sd))
  # suppressWarnings here used to prevent warnings about ties
  ks1 <- suppressWarnings(stats::ks.test(lognormal_draws$meanlog, samples1))
  ks2 <- suppressWarnings(stats::ks.test(lognormal_draws$sdlog, samples2))
  testthat::expect_gt(ks1$p.value, 0.01)
  testthat::expect_gt(ks2$p.value, 0.01)
})

test_that("epidist.epidist_latent_individual fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  fit <- epidist(data = prep_obs, seed = 1)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_latent_individual recovers the simulation settings for the delay distribution in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  fit <- epidist(data = prep_obs, seed = 1)
  lognormal_draws <- extract_lognormal_draws(fit)
  # Unclear the extent to which we should expect parameter recovery here
  expect_equal(mean(lognormal_draws$meanlog), meanlog, tolerance = 0.1)
  expect_equal(mean(lognormal_draws$sdlog), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_latent_individual Stan code has no syntax errors and compiles in the gamma delay case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode_gamma <- epidist(
    data = prep_obs_gamma,
    family = epidist_family(prep_obs_gamma, family = "gamma"),
    fn = brms::make_stancode
  )
  mod_gamma <- cmdstan_model(
    stan_file = write_stan_file(stancode_gamma), compile = FALSE
  )
  expect_true(mod_gamma$check_syntax())
  expect_no_error(mod_gamma$compile())
})

test_that("epidist.epidist_latent_individual fits and the MCMC converges  in the gamma delay case", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  
  gamma_family <- epidist_family(
    prep_obs_gamma,
    family = "gamma",
    dpars = c("mu", "shape"),
    links = c("log", "log"),
    lb = c(0, 0),
    ub = c(NA, NA)
  )

  gamma_formula <- epidist_formula(prep_obs_gamma)
  gamma_formula$pforms$sigma <- NULL
  gamma_formula <- brms::bf(gamma_formula, as.formula("shape ~ 0"))
  
  fit_gamma <- epidist(
    data = prep_obs_gamma,
    family = gamma_family,
    prior = brms::prior("normal(0, 3)", class = "Intercept"),
    formula = gamma_formula,
    seed = 1
  )
  
  expect_s3_class(fit_gamma, "brmsfit")
  expect_s3_class(fit_gamma, "epidist_fit")
  expect_convergence(fit_gamma)
})

test_that("epidist.epidist_latent_individual recovers the simulation settings for the delay distribution in the gamma delay case", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  
  gamma_family <- epidist_family(
    prep_obs_gamma,
    family = "gamma",
    dpars = c("mu", "shape"),
    links = c("log", "log"),
    lb = c(0, 0),
    ub = c(NA, NA)
  )
  
  gamma_formula <- epidist_formula(prep_obs_gamma)
  gamma_formula$pforms$sigma <- NULL
  gamma_formula <- brms::bf(gamma_formula, as.formula("shape ~ 1"))
  
  # Fix the shape intercept to log(2) so that it corresponds to simulated value
  gamma_prior <- brms::prior("normal(0, 3)", class = "Intercept") +
    brms::prior("constant(0.6931472)", class = "Intercept", dpar = "shape")
  
  fit_gamma <- epidist(
    data = prep_obs_gamma,
    family = gamma_family,
    prior = gamma_prior,
    formula = gamma_formula,
    seed = 1
  )
  
  # What the value is of the delay i.e. mu = 2/3
  mean(prep_obs_gamma$delay)
  
  draws_gamma <- posterior::as_draws_df(fit_gamma$fit)
  draws_gamma_mu <- exp(draws_gamma$Intercept)
  
  expect_equal(mean(draws_gamma_mu), mu, tolerance = 0.1)
})

test_that("epidist.epidist_latent_individual Stan code has no syntax errors and compiles for an alternative formula", { # nolint: line_length_linter.
  skip_on_cran()
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  stancode_sex <- epidist(data = prep_obs, formula = formula_sex,
                          fn = brms::make_stancode)
  mod_sex <- cmdstan_model(
    stan_file = write_stan_file(stancode_sex), compile = FALSE
  )
  expect_true(mod_sex$check_syntax())
  expect_no_error(mod_sex$compile())
})

test_that("epidist.epidist_latent_individual recovers no sex effect when none is simulated", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  fit_sex <- epidist(data = prep_obs, formula = formula_sex, seed = 1)
  draws <- posterior::as_draws_df(fit_sex$fit)
  expect_equal(mean(draws$b_sex), 0, tolerance = 0.2)
  expect_equal(mean(draws$b_sigma_sex), 0, tolerance = 0.2)
})

test_that("epidist.epidist_latent_individual fits and the MCMC converges for an alternative formula", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  fit_sex <- epidist(data = prep_obs, formula = formula_sex, seed = 1)
  expect_s3_class(fit_sex, "brmsfit")
  expect_s3_class(fit_sex, "epidist_fit")
  expect_convergence(fit_sex)
})
