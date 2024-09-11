test_that("epidist_prior with default settings produces an object of the right class", { # nolint: line_length_linter.
  data <- as_latent_individual(sim_obs)
  family <- brms::lognormal()
  formula <- brms::bf(mu ~ 1, sigma ~ 1)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family(data, family), formula = formula
  )
  prior <- epidist_prior(data, family, formula = epidist_formula, prior = NULL)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})
