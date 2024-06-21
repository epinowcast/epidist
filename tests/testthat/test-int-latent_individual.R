prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")
prep_obs_gamma <- epidist_prepare(sim_obs_gamma, model = "latent_individual")

test_that("epidist.epidist_latent_individual Stan code compiles in the default case", { # nolint: line_length_linter.
  stancode <- epidist(data = prep_obs, fn = brms::make_stancode)
  mod <- cmdstan_model(stan_file = write_stan_file(stancode), compile = FALSE)
  expect_no_error(mod$compile())
})

test_that("epidist.epidist_latent_individual fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  fit <- epidist(data = prep_obs)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_latent_individual Stan code compiles in the gamma delay case", { # nolint: line_length_linter.
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

test_that("epidist.epidist_latent_individual fits and the MCMC converges for a gamma delay distribution", { # nolint: line_length_linter.
  fit_gamma <- epidist(
    data = prep_obs_gamma,
    family = epidist_family(prep_obs_gamma, family = "gamma"),
  )
  expect_s3_class(fit_gamma, "brmsfit")
  expect_s3_class(fit_gamma, "epidist_fit")
  expect_convergence(fit_gamma)
})

test_that("epidist.epidist_latent_individual Stan code compiles for an alternative formula", { # nolint: line_length_linter.
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

test_that("epidist.epidist_latent_individual fits and the MCMC converges for an alternative formula", { # nolint: line_length_linter.
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  formula_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                                 sigma = ~ 1 + sex)
  fit_sex <- epidist(data = prep_obs, formula = formula_sex)
  expect_s3_class(fit_sex, "brmsfit")
  expect_s3_class(fit_sex, "epidist_fit")
  expect_convergence(fit_sex)
})
