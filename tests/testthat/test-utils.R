test_that(".floor_mult works as expected, including with one and zero as f", { # nolint: line_length_linter.
  expect_equal(.floor_mult(1.5, 0.2), 1.4, tolerance = 1e-08)
  expect_identical(.floor_mult(1.5, 1), floor(1.5))
  expect_identical(.floor_mult(1.5, 0), 1.5)
  expect_error(.floor_mult(1.5, -1))
})

test_that(".replace_prior successfully replaces priors", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma")
  prior <- .replace_prior(old_prior, new_prior)
  expect_identical(prior$prior, c("normal(0, 5)", "normal(0, 5)"))
  expect_identical(as.double(nrow(prior)), 2)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

cli::test_that_cli(".replace_prior errors when passed a new prior without a match in old_prior", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "shape")

  expect_snapshot({
    .replace_prior(old_prior, new_prior, warn = TRUE)
  })
})

test_that(".add_dpar_info works as expected for the lognormal and gamma families", { # nolint: line_length_linter.
  lognormal_extra <- .add_dpar_info(brms::lognormal())
  expect_identical(lognormal_extra$other_links, "log")
  expect_identical(lognormal_extra$other_bounds, list(list(lb = "0", ub = "")))
  gamma_extra <- .add_dpar_info(brms:::validate_family(stats::Gamma())) # nolint
  expect_null(gamma_extra$other_links)
  expect_identical(gamma_extra$other_bounds, list(list(lb = "0", ub = "")))
})

test_that(".make_intercepts_explicit creates a formula which is the same as if it had been explicitly created", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  epidist_family <- epidist_family(prep_obs, family = lognormal())
  formula <- brms:::validate_formula( # nolint
    formula = bf(mu ~ 1),
    data = prep_obs,
    family = epidist_family
  )
  formula <- .make_intercepts_explicit(formula)
  formula_explicit <- brms:::validate_formula( # nolint
    formula = bf(mu ~ 1, sigma ~ 1),
    data = prep_obs,
    family = epidist_family
  )
  attr(formula$pforms$sigma, ".Environment") <- NULL
  attr(formula_explicit$pforms$sigma, ".Environment") <- NULL
  expect_identical(formula, formula_explicit)
})

test_that(".make_intercepts_explicit does not add an intercept if the distributional parameter is set to be fixed", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  epidist_family <- epidist_family(prep_obs, family = lognormal())
  formula <- brms:::validate_formula( # nolint
    formula = bf(mu ~ 1, sigma = 1),
    data = prep_obs,
    family = epidist_family
  )
  formula_updated <- .make_intercepts_explicit(formula)
  attr(formula$pforms$sigma, ".Environment") <- NULL
  attr(formula_updated$pforms$sigma, ".Environment") <- NULL
  expect_identical(formula, formula_updated)
})
