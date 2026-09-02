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
    epidist_prior(data, epidist_family, epidist_formula, prior = user_prior)
  )
  prior <- epidist_prior(
    data, epidist_family, epidist_formula,
    prior = user_prior
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

test_that("epidist_model_prior centres a summaries only meta model on the reported means", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_estimates, Gamma(link = "log"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- epidist_model_prior(prep_meta_estimates, formula)
  expect_s3_class(prior, "brmsprior")
  expect_identical(nrow(prior), 1L)
  expect_identical(prior$class, "Intercept")
  expect_identical(prior$dpar, "")
  # sim_estimates reports means of 7.5 and 6.4
  centre <- signif(log(stats::median(c(7.5, 6.4))), 3)
  expect_identical(prior$prior, sprintf("normal(%s, 1)", centre))
})

test_that("epidist_prior for a Gamma summaries only meta fit is not centred on the placeholder response", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_estimates, Gamma(link = "log"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- suppressWarnings(
    epidist_prior(prep_meta_estimates, family, formula, prior = NULL)
  )
  intercept <- prior[prior$class == "Intercept" & !nzchar(prior$dpar), ]
  expect_identical(nrow(intercept), 1L)
  expect_identical(intercept$source, "model")
  expect_match(intercept$prior, "normal(1.9", fixed = TRUE)
  expect_false(any(grepl("-2.3", prior$prior, fixed = TRUE)))
})

test_that("epidist_model_prior for the meta model follows the link of mu", {
  family <- epidist_family(prep_meta_estimates, Gamma(link = "identity"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- epidist_model_prior(prep_meta_estimates, formula)
  expect_identical(prior$prior, "normal(6.95, 1)")
  family <- epidist_family(prep_meta_estimates, Gamma(link = "inverse"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  expect_null(epidist_model_prior(prep_meta_estimates, formula))
})

test_that("epidist_model_prior for the meta model falls back on quantiles and then individual delays", { # nolint: line_length_linter.
  quantiles_only <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"), type = "quantile", value = c(5, 9),
    p = c(0.25, 0.75), n = 100, relative_obs_time = Inf,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = quantiles_only))
  family <- epidist_family(meta, Gamma(link = "log"))
  formula <- epidist_formula(meta, family, bf(mu ~ 1))
  prior <- epidist_model_prior(meta, formula)
  expect_identical(prior$prior, sprintf("normal(%s, 1)", signif(log(7), 3)))

  sd_only <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = "sd", value = 3, n = 100, relative_obs_time = Inf,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  mixed <- suppressMessages(
    as_epidist_meta_model(sim_obs, estimates = sd_only)
  )
  prior <- epidist_model_prior(mixed, formula)
  individual <- mixed$obs_type == 1L
  delays <- stats::weighted.mean(
    mixed$delay_lwr[individual] + mixed$swindow[individual] / 2,
    mixed$n[individual]
  )
  expect_identical(
    prior$prior, sprintf("normal(%s, 1)", signif(log(delays), 3))
  )
})

test_that("epidist_model_prior adds nothing to a meta model of individual rows only", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_individual, Gamma(link = "log"))
  formula <- epidist_formula(prep_meta_individual, family, bf(mu ~ 1))
  expect_null(epidist_model_prior(prep_meta_individual, formula))
})
