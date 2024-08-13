test_that("replace_prior successfully replaces priors", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma")
  prior <- replace_prior(old_prior, new_prior)
  expect_equal(prior$prior, c("normal(0, 5)", "normal(0, 5)"))
  expect_equal(nrow(prior), 2)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

test_that("replace_prior errors when passed a new prior without a match in old_prior", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "shape")
  expect_error(replace_prior(old_prior, new_prior))
})

test_that("epidist_prior with default settings produces an object of the right class", { # nolint: line_length_linter.
  data <- as_latent_individual(sim_obs)
  family <- brms::lognormal()
  formula <- brms::bf(mu ~ 1, sigma ~ 1)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family(data, family), formula = formula
  )
  class(family) <- c(class(family), family$family)
  prior <- epidist_prior(data, family, formula = epidist_formula, prior = NULL)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})
