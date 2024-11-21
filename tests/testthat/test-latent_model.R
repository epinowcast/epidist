test_that("as_epidist_latent_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_model")
})

test_that("as_epidist_latent_model.epidist_linelist_data errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_latent_model(list()))
  expect_error(as_epidist_latent_model(sim_obs[, 1]))
})

# Make this data available for other tests
family_lognormal <- epidist_family(prep_obs, family = lognormal())

test_that("is_epidist_latent_model returns TRUE for correct input", { # nolint: line_length_linter.
  expect_true(is_epidist_latent_model(prep_obs))
  expect_true({
    x <- list()
    class(x) <- "epidist_latent_model"
    is_epidist_latent_model(x)
  })
})

test_that("is_epidist_latent_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_false(is_epidist_latent_model(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_latent_model_extension"
    is_epidist_latent_model(x)
  })
})

test_that("assert_epidist.epidist_latent_model doesn't produce an error for correct input", { # nolint: line_length_linter.
  expect_no_error(assert_epidist(prep_obs))
})

test_that("assert_epidist.epidist_latent_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_error(assert_epidist(list()))
  expect_error(assert_epidist(prep_obs[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_latent_model"
    assert_epidist(x)
  })
})

test_that("epidist_stancode.epidist_latent_model produces valid stanvars", { # nolint: line_length_linter.
  epidist_family <- epidist_family(prep_obs)
  epidist_formula <- epidist_formula(
    prep_obs, epidist_family,
    formula = bf(mu ~ 1)
  )
  stancode <- epidist_stancode(
    prep_obs,
    family = epidist_family, formula = epidist_formula
  )
  expect_s3_class(stancode, "stanvars")
})

test_that("epidist_gen_log_lik_latent returns a function that produces valid log likelihoods", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  prep <- brms::prepare_predictions(fit)
  i <- 1
  log_lik_fn <- epidist_gen_log_lik_latent(lognormal())
  log_lik <- log_lik_fn(i = i, prep)
  expect_length(log_lik, prep$ndraws)
  expect_false(anyNA(log_lik))
  expect_true(all(is.finite(log_lik)))

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  log_lik_fn_gamma <- epidist_gen_log_lik_latent(Gamma())
  log_lik_gamma <- log_lik_fn_gamma(i = i, prep_gamma)
  expect_length(log_lik_gamma, prep_gamma$ndraws)
  expect_false(anyNA(log_lik_gamma))
  expect_true(all(is.finite(log_lik_gamma)))
})
