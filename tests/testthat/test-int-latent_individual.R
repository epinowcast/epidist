prep_obs <- as_latent_individual(sim_obs)
prep_obs_gamma <- as_latent_individual(sim_obs_gamma)

test_that("epidist.epidist_latent_individual Stan code compiles in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(data = prep_obs, fn = brms::make_stancode)
  mod <- cmdstan_model(stan_file = write_stan_file(stancode), compile = FALSE)
  expect_no_error(mod$compile())
})

extract_normal_parameters_brms <- function(prior) {
  pattern <- "normal\\(([^,]+), ([^\\)]+)\\)"
  match <- regmatches(prior, regexec(pattern, prior))
  mean <- as.numeric(match[[1]][2])
  sd <- as.numeric(match[[1]][3])
  return(list(mean = mean, sd = sd))
}

test_that("epidist.epidist_latent_individual samples from the prior according to marginal Kolmogorov-Smirnov tests in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  prior_samples <- epidist(data = prep_obs, fn = brms::brm,
                           sample_prior = "only")
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
  skip_on_cran()
  fit <- epidist(data = prep_obs)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_latent_individual recovers the simulation settings for the delay distribution in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  fit <- epidist(data = prep_obs)
  lognormal_draws <- extract_lognormal_draws(fit)
  # Unclear the extent to which we should expect parameter recovery here
  expect_equal(mean(lognormal_draws$meanlog), meanlog, tolerance = 0.1)
  expect_equal(mean(lognormal_draws$sdlog), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_latent_individual Stan code compiles in the gamma delay case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode_gamma <- epidist(
    data = prep_obs_gamma,
    family = epidist_family(prep_obs_gamma, family = "gamma"),
    fn = brms::make_stancode
  )
  mod_gamma <- cmdstan_model(
    stan_file = write_stan_file(stancode_gamma), compile = FALSE
  )
  expect_no_error(mod_gamma$compile())
})

test_that("epidist.epidist_latent_individual Stan code compiles for an alternative formula", { # nolint: line_length_linter.
  skip_on_cran()
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  stancode_sex <- epidist(data = prep_obs, formula = formula_sex,
                          fn = brms::make_stancode)
  mod_sex <- cmdstan_model(
    stan_file = write_stan_file(stancode_sex), compile = FALSE
  )
  expect_no_error(mod_sex$compile())
})

test_that("epidist.epidist_latent_individual recovers no sex effect when none is simulated", { # nolint: line_length_linter.
  skip_on_cran()
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  fit_sex <- epidist(data = prep_obs, formula = formula_sex)
  draws <- posterior::as_draws_df(fit_sex$fit)
  expect_equal(mean(draws$b_sex), 0, tolerance = 0.2)
  expect_equal(mean(draws$b_sigma_sex), 0, tolerance = 0.2)
})

test_that("epidist.epidist_latent_individual fits and the MCMC converges for an alternative formula", { # nolint: line_length_linter.
  skip_on_cran()
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  fit_sex <- epidist(data = prep_obs, formula = formula_sex)
  expect_s3_class(fit_sex, "brmsfit")
  expect_s3_class(fit_sex, "epidist_fit")
  expect_convergence(fit_sex)
})
