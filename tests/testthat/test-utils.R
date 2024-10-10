test_that(".floor_mult works as expected, including with one and zero as f", { # nolint: line_length_linter.
  expect_equal(.floor_mult(1.5, 0.2), 1.4)
  expect_equal(.floor_mult(1.5, 1), floor(1.5))
  expect_equal(.floor_mult(1.5, 0), 1.5)
  expect_error(.floor_mult(1.5, -1))
})

test_that(".replace_prior successfully replaces priors", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma")
  prior <- .replace_prior(old_prior, new_prior)
  expect_equal(prior$prior, c("normal(0, 5)", "normal(0, 5)"))
  expect_equal(nrow(prior), 2)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

test_that(".replace_prior errors when passed a new prior without a match in old_prior", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "shape")
  expect_error(.replace_prior(old_prior, new_prior))
})

test_that(".add_dpar_info works as expected for the lognormal and gamma families", { # nolint: line_length_linter.
  lognormal_extra <- .add_dpar_info(brms::lognormal())
  expect_equal(lognormal_extra$other_links, "log")
  expect_equal(lognormal_extra$other_bounds, list(list("lb" = "0", ub = "")))
  gamma_extra <- .add_dpar_info(brms:::validate_family(stats::Gamma()))
  expect_equal(gamma_extra$other_links, NULL)
  expect_equal(gamma_extra$other_bounds, list(list("lb" = "0", ub = "")))
})

test_that(".make_intercepts_explicit creates a formula which is the same as if it had been explicitly created", { # nolint: line_length_linter.
  prep_obs <- as_latent_individual(sim_obs)
  epidist_family <- epidist_family(prep_obs, family = "lognormal")
  formula <- brms:::validate_formula(
    formula = brms::bf(mu ~ 1),
    data = prep_obs,
    family = epidist_family
  
  formula <- .make_intercepts_explicit(formula)
  formula_explicit <- brms:::validate_formula(
    formula = brms::bf(mu ~ 1, sigma ~ 1),
    data = prep_obs,
    family = epidist_family
  )
  expect_equal(formula, formula_explicit)
})
