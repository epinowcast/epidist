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

test_that("epidist_prior does not warn about user priors on model coefficients", { # nolint: line_length_linter.
  data <- as_epidist_latent_model(sim_obs_sex)
  family <- lognormal()
  formula <- bf(mu ~ 1 + sex, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  user_prior <- prior("normal(0, 1)", class = "b")
  expect_no_warning(
    prior <- epidist_prior(
      data, epidist_family, epidist_formula,
      prior = user_prior
    )
  )
  expect_true("normal(0, 1)" %in% prior$prior)
})

test_that("epidist_prior does not warn about user priors on manual parameters", { # nolint: line_length_linter.
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  user_prior <- prior("pwindow_raw ~ uniform(0, 1);", check = FALSE)
  expect_no_warning(
    epidist_prior(data, epidist_family, epidist_formula, prior = user_prior)
  )
})

test_that("epidist_prior combines default, family and model priors in order", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  prior <- epidist_prior(data, epidist_family, epidist_formula, prior = NULL)

  expect_identical(
    prior$prior,
    c(
      "normal(1, 1)", "normal(-0.7, 0.4)",
      "pwindow_raw ~ uniform(0, 1);", "swindow_raw ~ uniform(0, 1);"
    )
  )
  expect_identical(prior$class, c("Intercept", "Intercept", "", ""))
  expect_identical(prior$dpar, c("", "sigma", "", ""))
  expect_identical(prior$source, c("family", "family", "model", "model"))
})

test_that("epidist_prior with merge = FALSE only uses user priors", {
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
    prior = user_prior, merge = FALSE
  )
  expect_identical(prior, user_prior)
})

test_that("epidist_prior with enforce_presence = TRUE drops unmatched defaults", { # nolint: line_length_linter.
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
    prior = user_prior, enforce_presence = TRUE
  )
  expect_identical(
    prior$prior,
    c(
      "normal(0,1)",
      "pwindow_raw ~ uniform(0, 1);", "swindow_raw ~ uniform(0, 1);"
    )
  )
})

test_that("epidist_prior lets user manual priors replace model manual priors", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  user_prior <- prior("pwindow_raw ~ uniform(0, 1);", check = FALSE)
  prior <- epidist_prior(
    data, epidist_family, epidist_formula,
    prior = user_prior
  )
  expect_identical(
    prior$prior,
    c(
      "normal(1, 1)", "normal(-0.7, 0.4)",
      "pwindow_raw ~ uniform(0, 1);", "swindow_raw ~ uniform(0, 1);"
    )
  )
  expect_identical(prior$source, c("family", "family", "user", "model"))
})
