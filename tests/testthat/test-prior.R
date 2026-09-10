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

test_that("epidist_model_prior puts a fixed prior on the intercept of a summaries only meta model", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_estimates, Gamma(link = "log"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- epidist_model_prior(prep_meta_estimates, formula)
  expect_s3_class(prior, "brmsprior")
  expect_identical(nrow(prior), 3L)
  expect_identical(prior$class, c("Intercept", "sd", "sd"))
  expect_identical(prior$dpar, c("", "", "shape"))
  # The centre is not taken from the reported values (sim_estimates reports
  # means of 7.5 and 6.4), but is the scale of the lognormal family prior.
  expect_identical(prior$prior[1], "normal(1, 1)")
})

test_that("epidist_model_prior puts a half normal on the between study spread of a meta model", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_estimates, lognormal())
  grouped <- epidist_formula(
    prep_meta_estimates, family, bf(mu ~ 1 + (1 | study))
  )
  full <- suppressWarnings(
    epidist_prior(prep_meta_estimates, family, grouped, prior = NULL)
  )
  spread <- full[
    full$class == "sd" & !nzchar(full$coef) & !nzchar(full$group),
  ]
  expect_identical(nrow(spread), 1L)
  expect_identical(spread$prior, "normal(0, 0.25)")
  expect_identical(spread$source, "model")
  # A group level term on another distributional parameter gets the same.
  both <- epidist_formula(
    prep_meta_estimates, family,
    bf(mu ~ 1 + (1 | study), sigma ~ 1 + (1 | study))
  )
  full <- suppressWarnings(
    epidist_prior(prep_meta_estimates, family, both, prior = NULL)
  )
  spread <- full[
    full$class == "sd" & !nzchar(full$coef) & !nzchar(full$group),
  ]
  expect_identical(nrow(spread), 2L)
  expect_setequal(spread$dpar, c("", "sigma"))
  expect_true(all(spread$prior == "normal(0, 0.25)"))
  # Without a group level term the prior is dropped rather than warned about.
  flat <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  full <- suppressWarnings(
    epidist_prior(prep_meta_estimates, family, flat, prior = NULL)
  )
  expect_false(any(full$class == "sd"))
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
  expect_identical(intercept$prior, "normal(1, 1)")
  expect_false(any(grepl("-2.3", prior$prior, fixed = TRUE)))
})

test_that("epidist_model_prior puts the lognormal family scale on a lognormal meta model", { # nolint: line_length_linter.
  # The lognormal family gives mu an identity link, but mu is meanlog, so
  # the prior is on the log scale as it is for a log link.
  family <- epidist_family(prep_meta_estimates, lognormal())
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- epidist_model_prior(prep_meta_estimates, formula)
  expect_identical(prior$prior[1], "normal(1, 1)")
  full <- suppressWarnings(
    epidist_prior(prep_meta_estimates, family, formula, prior = NULL)
  )
  intercept <- full[full$class == "Intercept" & !nzchar(full$dpar), ]
  expect_identical(intercept$prior, "normal(1, 1)")
})

test_that("epidist_model_prior for the meta model adds nothing for other links", { # nolint: line_length_linter.
  # The fixed centre is on the log scale, so a family whose mu is the delay
  # itself, or its inverse, is left to the family or brms default.
  family <- epidist_family(prep_meta_estimates, Gamma(link = "identity"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  expect_null(epidist_model_prior(prep_meta_estimates, formula))
  family <- epidist_family(prep_meta_estimates, Gamma(link = "inverse"))
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  expect_null(epidist_model_prior(prep_meta_estimates, formula))
})

test_that("epidist_model_prior for the meta model does not depend on the reported values", { # nolint: line_length_linter.
  # A prior chosen from the data would put the posterior of a small review
  # where the data already sit, so the centre is the same whatever the
  # studies reported and whether individual level delays are present.
  quantiles_only <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"), type = "quantile", value = c(50, 90),
    p = c(0.25, 0.75), n = 100, relative_obs_time = Inf,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = quantiles_only))
  family <- epidist_family(meta, Gamma(link = "log"))
  formula <- epidist_formula(meta, family, bf(mu ~ 1))
  prior <- epidist_model_prior(meta, formula)
  expect_identical(prior$prior[1], "normal(1, 1)")

  sd_only <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = "sd", value = 3, n = 100, relative_obs_time = Inf,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  mixed <- suppressMessages(
    as_epidist_meta_model(sim_obs, estimates = sd_only)
  )
  prior <- epidist_model_prior(mixed, formula)
  expect_identical(prior$prior[1], "normal(1, 1)")
})

test_that("epidist_model_prior adds nothing to a meta model of individual rows only", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_individual, Gamma(link = "log"))
  formula <- epidist_formula(prep_meta_individual, family, bf(mu ~ 1))
  expect_null(epidist_model_prior(prep_meta_individual, formula))
})

test_that("epidist_model_prior gives the growth rate of a summary row a prior", { # nolint: line_length_linter.
  # Study A gives no rate, "site 2" a rate with a standard deviation whose
  # level brms strips the space from, and C a known rate.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A", "site 2", "site 2", "C", "C"),
    type = c("mean", "sd", "quantile", "quantile", "mean", "sd"),
    value = c(7.5, 3.6, 4.8, 8.6, 6.4, 3.0),
    p = c(NA, NA, 0.25, 0.75, NA, NA),
    n = 120,
    relative_obs_time = c(20, 20, 30, 30, 25, 25),
    trunc_adjusted = FALSE,
    trunc_design = c(
      "accrual", "accrual", "accrual", "accrual", "cohort", "cohort"
    ),
    cens_adjusted = c(0, 0, 0, 0, 2, 2),
    growth_rate = c(NA, NA, 0.1, 0.1, 0.05, 0.05),
    growth_rate_sd = c(NA, NA, 0.02, 0.02, NA, NA),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  family <- epidist_family(meta, lognormal())
  formula <- epidist_formula(meta, family, bf(mu ~ 1))
  prior <- epidist_model_prior(meta, formula)
  growth <- prior[prior$dpar == "pgrowth", ]
  expect_identical(growth$class, c("sd", "b", "Intercept", "b"))
  expect_identical(growth$coef, c("", "", "", "studysite2"))
  expect_identical(
    growth$prior,
    c(
      "normal(0, 0.25)", "normal(0, 0.25)", "normal(0, 0.25)",
      "normal(0.1, 0.02)"
    )
  )
  # Only the priors of coefficients the formula has survive the merge with
  # the brms defaults, and the reported rate reaches its own coefficient.
  full <- suppressWarnings(epidist_prior(meta, family, formula, prior = NULL))
  growth <- full[full$dpar == "pgrowth", ]
  expect_identical(growth$class, c("b", "b"))
  expect_identical(growth$coef, c("", "studysite2"))
  expect_identical(growth$prior, c("normal(0, 0.25)", "normal(0.1, 0.02)"))
  expect_true(all(growth$source == "model"))
  code <- suppressMessages(epidist(meta, fn = brms::make_stancode))
  expect_match(code, "normal_lpdf(b_pgrowth[3] | 0.1, 0.02)", fixed = TRUE)
  # A user prior on the coefficient wins.
  user <- suppressWarnings(epidist_prior(
    meta, family, formula,
    prior = prior(normal(0.2, 0.05),
      class = "b", coef = "studysite2",
      dpar = "pgrowth"
    )
  ))
  expect_identical(
    user$prior[user$dpar == "pgrowth" & user$coef == "studysite2"],
    "normal(0.2, 0.05)"
  )
  # The growth priors do not depend on the link of mu.
  gamma <- epidist_family(meta, Gamma(link = "identity"))
  gamma_formula <- epidist_formula(meta, gamma, bf(mu ~ 1))
  prior <- epidist_model_prior(meta, gamma_formula)
  expect_true(all(prior$dpar == "pgrowth"))
  expect_true("studysite2" %in% prior$coef)
})

test_that("epidist_model_prior warns where a reported growth rate has no coefficient", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A", "B", "B"),
    type = c("mean", "sd", "mean", "sd"),
    value = c(7.5, 3.6, 6.4, 3.0),
    n = 120,
    relative_obs_time = c(20, 20, 25, 25),
    trunc_adjusted = FALSE,
    trunc_design = "accrual",
    cens_adjusted = 0,
    growth_rate = c(NA, NA, 0.1, 0.1),
    growth_rate_sd = c(NA, NA, 0.02, 0.02),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  family <- epidist_family(meta, lognormal())
  shared <- epidist_formula(meta, family, bf(mu ~ 1, pgrowth ~ 1))
  expect_warning(epidist_model_prior(meta, shared), "no coefficient")
  prior <- suppressWarnings(epidist_model_prior(meta, shared))
  growth <- prior[prior$dpar == "pgrowth" & prior$class != "sd", ]
  expect_identical(growth$class, c("b", "Intercept"))
  expect_false(any(nzchar(growth$coef)))
  full <- suppressWarnings(
    epidist_prior(meta, family, shared, prior = NULL)
  )
  intercept <- full[full$dpar == "pgrowth" & full$class == "Intercept", ]
  expect_identical(intercept$prior, "normal(0, 0.25)")
  # A study without a standard deviation has nothing to warn about.
  estimates$growth_rate_sd <- NA
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_no_warning(epidist_model_prior(meta, shared))
  # The only study of a model puts its reported rate on the intercept. The
  # rate is set first, because a standard deviation without one is an error.
  estimates$growth_rate <- 0.1
  estimates$growth_rate_sd <- 0.02
  single <- suppressMessages(
    as_epidist_meta_model(estimates = estimates[estimates$study == "B", ])
  )
  family <- epidist_family(single, lognormal())
  formula <- epidist_formula(single, family, bf(mu ~ 1))
  expect_identical(as_string_formula(formula$pforms$pgrowth), "pgrowth ~ 1")
  prior <- epidist_model_prior(single, formula)
  intercept <- prior[prior$dpar == "pgrowth" & prior$class == "Intercept", ]
  expect_identical(intercept$prior, "normal(0.1, 0.02)")
  full <- suppressWarnings(epidist_prior(single, family, formula, prior = NULL))
  intercept <- full[full$dpar == "pgrowth" & full$class == "Intercept", ]
  expect_identical(intercept$prior, "normal(0.1, 0.02)")
})

test_that("epidist_model_prior matches a study id brms mangles by substitution", { # nolint: line_length_linter.
  # brms turns a hyphen into "M" rather than stripping it, so "site-2"
  # becomes "studysiteM2". Matching the reported rate positionally against
  # the levels of the study factor, rather than re-deriving the mangling,
  # finds that coefficient without a warning.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A", "site-2", "site-2"),
    type = c("mean", "sd", "mean", "sd"),
    value = c(7.5, 3.6, 6.4, 3.0),
    n = 120,
    relative_obs_time = c(20, 20, 25, 25),
    trunc_adjusted = FALSE,
    trunc_design = "accrual",
    cens_adjusted = 0,
    growth_rate = c(NA, NA, 0.1, 0.1),
    growth_rate_sd = c(NA, NA, 0.02, 0.02),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  family <- epidist_family(meta, lognormal())
  formula <- epidist_formula(meta, family, bf(mu ~ 1))
  prior <- epidist_model_prior(meta, formula)
  expect_no_warning(epidist_model_prior(meta, formula))
  growth <- prior[prior$dpar == "pgrowth" & prior$class == "b", ]
  expect_true("studysiteM2" %in% growth$coef)
  expect_identical(
    growth$prior[growth$coef == "studysiteM2"], "normal(0.1, 0.02)"
  )
})

test_that("epidist_model_prior adds no growth prior where every rate is known", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_estimates, lognormal())
  formula <- epidist_formula(prep_meta_estimates, family, bf(mu ~ 1))
  prior <- epidist_model_prior(prep_meta_estimates, formula)
  expect_false(any(prior$dpar == "pgrowth"))
  # Individual level rows with an exponential growth primary event are left
  # to the family or brms default, as in the marginal model.
  growing <- suppressMessages(
    as_epidist_meta_model(sim_obs, primary = "expgrowth")
  )
  family <- epidist_family(growing, lognormal())
  formula <- epidist_formula(growing, family, bf(mu ~ 1, pgrowth ~ 1))
  expect_null(epidist_model_prior(growing, formula))
})
