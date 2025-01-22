test_that("as_epidist_marginal_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
  expect_s3_class(prep_marginal_obs, "data.frame")
  expect_s3_class(prep_marginal_obs, "epidist_marginal_model")
})

test_that("as_epidist_marginal_model.epidist_linelist_data errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_marginal_model(list()))
  expect_error(as_epidist_marginal_model(sim_obs[, 1]))
})

test_that("as_epidist_marginal_model.epidist_linelist_data respects weighting variable", { # nolint: line_length_linter.
  # Create test data with a weighting column
  weighted_data <- sim_obs
  weighted_data$weight <- rep(c(1, 2), length.out = nrow(weighted_data))

  # Check weighted model has correct n values
  weighted_model <- as_epidist_marginal_model(
    weighted_data, weighting = "weight"
  )
  expect_identical(weighted_model$n, weighted_data$weight)

  # Check unweighted model has n=1
  unweighted_model <- as_epidist_marginal_model(sim_obs)
  expect_true(all(unweighted_model$n == 1))
})

test_that(
  "as_epidist_marginal_model.epidist_linelist_data errors with invalid weighting column", { # nolint: line_length_linter.
  expect_error(
    as_epidist_marginal_model(sim_obs, weighting = "nonexistent_column"),
    regexp = "Names must include the elements"
  )
})

# Make this data available for other tests
family_lognormal <- epidist_family(prep_marginal_obs, family = lognormal())

test_that("is_epidist_marginal_model returns TRUE for correct input", { # nolint: line_length_linter.
  expect_true(is_epidist_marginal_model(prep_marginal_obs))
  expect_true({
    x <- list()
    class(x) <- "epidist_marginal_model"
    is_epidist_marginal_model(x)
  })
})

test_that("is_epidist_marginal_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_false(is_epidist_marginal_model(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_marginal_model_extension"
    is_epidist_marginal_model(x)
  })
})

test_that("assert_epidist.epidist_marginal_model doesn't produce an error for correct input", { # nolint: line_length_linter.
  expect_no_error(assert_epidist(prep_marginal_obs))
})

test_that("assert_epidist.epidist_marginal_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_error(assert_epidist(list()))
  expect_error(assert_epidist(prep_marginal_obs[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_marginal_model"
    assert_epidist(x)
  })
})

test_that("epidist_stancode.epidist_marginal_model produces valid stanvars", { # nolint: line_length_linter.
  epidist_family <- epidist_family(prep_marginal_obs)
  epidist_formula <- epidist_formula(
    prep_marginal_obs, epidist_family,
    formula = bf(mu ~ 1)
  )
  stancode <- epidist_stancode(
    prep_marginal_obs,
    family = epidist_family, formula = epidist_formula
  )
  expect_s3_class(stancode, "stanvars")
})

test_that("epidist_transform_data_model.epidist_marginal_model correctly transforms data and messages", { # nolint: line_length_linter.
  family <- epidist_family(prep_marginal_obs, family = lognormal())
  formula <- epidist_formula(
    prep_marginal_obs,
    formula = bf(mu ~ 1),
    family = family
  )
  expect_no_message(
    expect_message(
      expect_message(
        expect_message(
          epidist_transform_data_model(
            prep_marginal_obs,
            family = family,
            formula = formula
          ),
          "Reduced from 500 to 144 rows."
        ),
        "Data summarised by unique combinations of:"
      ),
      "Model variables"
    )
  )

  family <- epidist_family(prep_marginal_obs, family = lognormal())
  formula <- epidist_formula(
    prep_marginal_obs,
    formula = bf(mu ~ 1 + ptime_lwr),
    family = family
  )
  expect_message(
    expect_message(
      expect_message(
        expect_message(
          epidist_transform_data_model(
            prep_marginal_obs,
            family = family,
            formula = formula
          ),
          "Reduced from 500 to 144 rows."
        ),
        "Data summarised by unique combinations of:"
      ),
      "Model variables"
    ),
    "ptime_lwr"
  )
})
