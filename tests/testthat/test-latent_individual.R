# Assumes that tests/testthat/setup.R has been run

# Generate observation data in correct format for the latent_individual model
prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

test_that("epidist_formula.epidist_latent_individual with default settings produces a brmsformula with the correct intercept only formula", { # nolint: line_length_linter.
  form <- epidist_formula(prep_obs)
  expect_s3_class(form, "brmsformula")
  expect_equal(
    deparse(form$formula),
    "delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1"
  )
  expect_equal(
    deparse(form$pforms$sigma),
    "sigma ~ 1"
  )
})

test_that("epidist_family.epidist_latent_individual with default settings produces an object of the right class", { # nolint: line_length_linter.
  family <- epidist_family(prep_obs)
  expect_s3_class(family, "customfamily")
  expect_s3_class(family, "brmsfamily")
  expect_s3_class(family, "family")
})

priors <- epidist_priors(prep_obs)
# TODO: add tests for priors: waiting on prior PR being merged for this
priors

test_that("epidist_stancode.epidist_latent_individual", { # nolint: line_length_linter.
  stancode <- epidist_stancode(prep_obs)
  expect_equal(1, 1)
})

test_that("epidist.epidist_latent_individual fits in the default case", {
  fit <- epidist(data = prep_obs)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

# Could also do tests on the stancode?
stancode_external <- epidist(prep_obs, fn = brms::make_stancode)

test_that("epidist.epidist_latent_individual fits for a gamma delay distribution", { # nolint: line_length_linter.
  fit_gamma <- epidist(
    data = prep_obs,
    family = epidist_family(prep_obs, family = "gamma"),
    stancode = epidist_stancode(
      prep_obs,
      family = epidist_family(prep_obs, family = "gamma")
    )
  )
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

# Could also test how default methods work
x <- list()
epidist_prepare(x)
epidist_family(x)
epidist_formula(x)
epidist_priors(x)
epidist_stancode(x)