test_that("as_latent_individual.epidist_linelist with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_latent_individual(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_individual")
})

test_that("as_latent_individual.epidist_linelist errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_latent_individual(list()))
  expect_error(as_latent_individual(sim_obs[, 1]))
})

# Make this data available for other tests
prep_obs <- as_latent_individual(sim_obs)
family_lognormal <- epidist_family(prep_obs, family = brms::lognormal())

test_that("is_latent_individual returns TRUE for correct input", { # nolint: line_length_linter.
  expect_true(is_latent_individual(prep_obs))
  expect_true({
    x <- list()
    class(x) <- "epidist_latent_individual"
    is_latent_individual(x)
  })
})

test_that("is_latent_individual returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_false(is_latent_individual(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_latent_individual_extension"
    is_latent_individual(x)
  })
})

test_that("epidist_validate_model.epidist_latent_individual doesn't produce an error for correct input", { # nolint: line_length_linter.
  expect_no_error(epidist_validate_model(prep_obs))
})

test_that("epidist_validate.epidist_latent_individual returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_error(epidist_validate(list()))
  expect_error(epidist_validate(prep_obs[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_latent_individual"
    epidist_validate(x)
  })
})

test_that("epidist_stancode.epidist_latent_individual produces valid stanvars", { # nolint: line_length_linter.
  epidist_family <- epidist_family(prep_obs)
  epidist_formula <- epidist_formula(
    prep_obs, epidist_family,
    formula = brms::bf(mu ~ 1)
  )
  stancode <- epidist_stancode(
    prep_obs,
    family = epidist_family, formula = epidist_formula
  )
  expect_s3_class(stancode, "stanvars")
})
