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
    brms::prior("normal(0, 2)", class = "Intercept", dpar = "sigma")
  prior <- .replace_prior(old_prior, new_prior)
  expect_identical(prior$prior, c("normal(0, 5)", "normal(0, 2)"))
  expect_identical(as.double(nrow(prior)), 2)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

cli::test_that_cli(".warn_unmatched_prior warns when passed a prior without a match", { # nolint: line_length_linter.
  known <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "shape")

  expect_warning(
    .warn_unmatched_prior(new_prior, known),
    "One or more priors have no match in existing parameters"
  )
})

test_that(".warn_unmatched_prior names the priors which have no match", {
  known <- brms::prior("normal(0, 10)", class = "Intercept")
  new_prior <- brms::prior("normal(0, 5)", class = "b", coef = "sex")

  expect_warning(
    .warn_unmatched_prior(new_prior, known),
    "class = b, coef = sex",
    fixed = TRUE
  )
})

test_that(".describe_prior gives the prior and the parameter it applies to", {
  prior <- brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("mu ~ normal(0, 5)", check = FALSE)
  expect_identical(
    .describe_prior(prior),
    c("normal(0, 5) (class = Intercept, dpar = sigma)", "mu ~ normal(0, 5)")
  )
})

test_that(".warn_unmatched_prior is silent when every prior matches", {
  known <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept")

  expect_no_warning(.warn_unmatched_prior(new_prior, known))
  expect_no_warning(.warn_unmatched_prior(NULL, known))
  expect_no_warning(.warn_unmatched_prior(new_prior, NULL))
})

test_that(".replace_prior handles custom ~ priors correctly", {
  # Create old priors with mix of standard and ~ syntax
  old_prior <- brms::prior("mu ~ normal(0, 10)", check = FALSE) +
    brms::prior("normal(0, 10)", dpar = "sigma") +
    brms::prior("beta ~ normal(0, 1)", check = FALSE)

  # Create new priors with ~ syntax
  new_prior <- brms::prior("mu ~ normal(0, 5)", check = FALSE) +
    brms::prior("gamma ~ normal(0, 2)", check = FALSE)

  # Test that only old priors with matching ~ parameter names are removed
  prior <- .replace_prior(old_prior, new_prior, enforce_presence = FALSE)

  # Should keep sigma prior, replace mu prior, keep beta prior, add gamma
  # prior
  expect_identical(
    prior$prior,
    c(
      "normal(0, 10)", "mu ~ normal(0, 5)", "gamma ~ normal(0, 2)",
      "beta ~ normal(0, 1)"
    )
  )
  expect_identical(as.double(nrow(prior)), 4)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})

test_that(".replace_prior handles priors which are all manual", {
  old_prior <- brms::prior("mu ~ normal(0, 10)", check = FALSE)
  new_prior <- brms::prior("mu ~ normal(0, 5)", check = FALSE) +
    brms::prior("gamma ~ normal(0, 2)", check = FALSE)

  prior <- .replace_prior(old_prior, new_prior)
  expect_identical(
    prior$prior,
    c("mu ~ normal(0, 5)", "gamma ~ normal(0, 2)")
  )
  expect_s3_class(prior, "brmsprior")
})

test_that(".replace_prior handles empty priors", {
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept")

  expect_identical(as.double(nrow(.replace_prior(
    old_prior, brms::empty_prior()
  ))), 0)
  expect_identical(
    .replace_prior(brms::empty_prior(), old_prior, enforce_presence = FALSE)$prior, # nolint: line_length_linter.
    "normal(0, 10)"
  )
})

test_that(".add_dpar_info works as expected for the lognormal and gamma families", { # nolint: line_length_linter.
  lognormal_extra <- .add_dpar_info(lognormal())
  expect_identical(lognormal_extra$other_links, "log")
  expect_identical(lognormal_extra$other_bounds, list(list(lb = "0", ub = "")))
  gamma_extra <- .add_dpar_info(.validate_family(Gamma()))
  expect_null(gamma_extra$other_links)
  expect_identical(gamma_extra$other_bounds, list(list(lb = "0", ub = "")))
})

test_that(".make_intercepts_explicit creates a formula which is the same as if it had been explicitly created", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  epidist_family <- epidist_family(prep_obs, family = lognormal())
  formula <- .validate_formula(
    formula = bf(mu ~ 1),
    data = prep_obs,
    family = epidist_family
  )
  formula <- .make_intercepts_explicit(formula)
  formula_explicit <- .validate_formula(
    formula = bf(mu ~ 1, sigma ~ 1),
    data = prep_obs,
    family = epidist_family
  )
  attr(formula$pforms$sigma, ".Environment") <- NULL
  attr(formula_explicit$pforms$sigma, ".Environment") <- NULL
  expect_identical(formula$pforms$mu, formula_explicit$pforms$mu)
  expect_identical(formula$pforms$sigma, formula_explicit$pforms$sigma)
})

test_that(".make_intercepts_explicit does not add an intercept if the distributional parameter is set to be fixed", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  epidist_family <- epidist_family(prep_obs, family = lognormal())
  formula <- .validate_formula(
    formula = bf(mu ~ 1, sigma = 1),
    data = prep_obs,
    family = epidist_family
  )
  formula_updated <- .make_intercepts_explicit(formula)
  expect_identical(formula$pforms$mu, formula_updated$pforms$mu)
  expect_identical(formula$pforms$sigma, formula_updated$pforms$sigma)
})

test_that(
  ".summarise_n_by_formula correctly summarizes counts by grouping variables",
  {
    df <- tibble::tibble(
      x = c(1, 1, 2, 2),
      y = c("a", "b", "a", "b"),
      n = c(2, 3, 4, 1)
    )

    # Test grouping by single variable
    result <- .summarise_n_by_formula(df, by = "x")
    expect_identical(result$x, c(1, 2))
    expect_identical(result$n, c(5, 5))

    # Test grouping by multiple variable
    result <- .summarise_n_by_formula(df, by = c("x", "y"))
    expect_identical(result$x, c(1, 1, 2, 2))
    expect_identical(result$y, c("a", "b", "a", "b"))
    expect_identical(result$n, c(2, 3, 4, 1))

    # Test with formula
    formula <- bf(mu ~ x + y)
    result <- .summarise_n_by_formula(df, formula = formula)
    expect_identical(result$x, c(1, 1, 2, 2))
    expect_identical(result$y, c("a", "b", "a", "b"))
    expect_identical(result$n, c(2, 3, 4, 1))

    # Test with both by and formula
    formula <- bf(mu ~ y)
    result <- .summarise_n_by_formula(df, by = "x", formula = formula)
    expect_identical(result$x, c(1, 1, 2, 2))
    expect_identical(result$y, c("a", "b", "a", "b"))
    expect_identical(result$n, c(2, 3, 4, 1))
  }
)

test_that(
  ".summarise_n_by_formula handles missing grouping variables appropriately",
  {
    df <- data.frame(x = 1:2, n = c(1, 2))
    expect_error(
      .summarise_n_by_formula(df, by = "missing"),
      "Can't subset elements that don't exist"
    )
  }
)

test_that(".summarise_n_by_formula requires n column in data", {
  df <- data.frame(x = 1:2)
  expect_error(
    .summarise_n_by_formula(df, by = "x"),
    "Column `n` not found in `.data`."
  )
})

test_that(".restore_compile_env restores set and unset variables", { # nolint: line_length_linter.
  # Leave the session as we found it, whatever the user had set.
  original <- .capture_compile_env()
  on.exit(.restore_compile_env(original))
  Sys.setenv(PKG_CPPFLAGS = "-Ifoo")
  Sys.unsetenv("PKG_LIBS")
  captured <- .capture_compile_env()
  expect_identical(unname(captured[["PKG_CPPFLAGS"]]), "-Ifoo")
  expect_true(is.na(captured[["PKG_LIBS"]]))

  Sys.setenv(PKG_CPPFLAGS = "-Ibar", PKG_LIBS = "-lbaz")
  .restore_compile_env(captured)
  expect_identical(Sys.getenv("PKG_CPPFLAGS"), "-Ifoo")
  expect_identical(Sys.getenv("PKG_LIBS", unset = NA), NA_character_)
})

test_that("epidist() does not leak the compilation variables rstan sets", { # nolint: line_length_linter.
  # Leave the session as we found it, whatever the user had set.
  original <- .capture_compile_env()
  on.exit(.restore_compile_env(original))
  Sys.unsetenv(c("PKG_CPPFLAGS", "PKG_LIBS"))
  leaky_fn <- function(...) {
    Sys.setenv(PKG_CPPFLAGS = "-include Eigen.hpp", PKG_LIBS = "-lStanHeaders")
    return(structure(list(), class = "brmsfit"))
  }
  epidist(prep_marginal_obs, fn = leaky_fn)
  expect_identical(Sys.getenv("PKG_CPPFLAGS", unset = NA), NA_character_)
  expect_identical(Sys.getenv("PKG_LIBS", unset = NA), NA_character_)
})
