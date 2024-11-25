test_that("epidist_prior with default settings produces an object of the right class", { # nolint: line_length_linter.
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )
  prior <- epidist_prior(data, epidist_family, epidist_formula, prior = NULL)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

test_that("epidist_prior correctly handles user-provided priors", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  user_prior <- prior("normal(0,1)", class = "Intercept")
  prior <- epidist_prior(
    data, epidist_family, epidist_formula,
    prior = user_prior
  )

  expect_identical(
    prior$prior[1],
    "normal(0,1)"
  )
})

test_that("epidist_prior warns about invalid user priors", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  invalid_prior <- prior("normal(0,1)", class = "InvalidClass")
  expect_warning(
    epidist_prior(data, epidist_family, epidist_formula, prior = invalid_prior),
    "One or more priors have no match in existing parameters"
  )
})

test_that("epidist_prior correctly applies family-specific priors", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  prior <- epidist_prior(data, epidist_family, epidist_formula, prior = NULL)

  expect_identical(
    prior$prior[1],
    "normal(1, 1)"
  )
  expect_identical(
    prior$prior[2],
    "normal(-0.7, 0.4)"
  )
})

test_that("epidist_prior correctly applies model-specific priors", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  prior <- epidist_prior(data, epidist_family, epidist_formula, prior = NULL)

  expect_true(
    any(prior$dpar == "pwindow" & prior$prior == "uniform(0, 1)")
  )
  expect_true(
    any(prior$dpar == "swindow" & prior$prior == "uniform(0, 1)")
  )
})
