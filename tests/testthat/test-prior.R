test_that("epidist_prior with default settings produces an object of the right class", { # nolint: line_length_linter.
  data <- as_epidist_latent_model(sim_obs)
  family <- brms::lognormal()
  formula <- brms::bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )
  prior <- epidist_prior(data, epidist_family, epidist_formula, prior = NULL)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})
