# fmt: skip file
test_that("as_epidist_naive_model.data.frame with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_epidist_naive_model(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_naive_model")
})

test_that("as_epidist_naive_model.data.frame errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_naive_model(list()))
  expect_error(as_epidist_naive_model(sim_obs[, 1]))
})

# Make this data available for other tests
family_lognormal <- epidist_family(sim_obs, family = lognormal())

test_that("is_epidist_naive_model returns TRUE for correct input", {
  expect_s3_class(prep_naive_obs, "epidist_naive_model")
  expect_s3_class(
    structure(list(), class = "epidist_naive_model"),
    "epidist_naive_model"
  )
})

test_that("is_epidist_naive_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_false(is_epidist_naive_model(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_naive_model_extension"
    is_epidist_naive_model(x)
  })
})

test_that("assert_epidist.epidist_naive_model doesn't produce an error for correct input", { # nolint: line_length_linter.
  expect_no_error(assert_epidist(prep_naive_obs))
})

test_that("assert_epidist.epidist_naive_model returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_error(assert_epidist(list()))
  expect_error(assert_epidist(prep_naive_obs[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_naive_model"
    assert_epidist(x)
  })
})

test_that("as_epidist_naive_model.epidist_aggregate_data example works", {
  result <- sierra_leone_ebola_data |>
    dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
    as_epidist_aggregate_data(
      pdate_lwr = "date_of_symptom_onset",
      sdate_lwr = "date_of_sample_tested",
      n = "n"
    ) |>
    as_epidist_naive_model()

  expect_s3_class(result, "epidist_naive_model")
  expect_s3_class(result, "data.frame")
  expect_true("delay" %in% names(result))
  expect_true(all(result$delay >= 0))
})

test_that("epidist_transform_data_model.epidist_naive_model correctly transforms data and messages", { # nolint: line_length_linter.
  family <- epidist_family(prep_naive_obs, family = lognormal())
  formula <- epidist_formula(
    prep_naive_obs,
    formula = bf(mu ~ 1),
    family = family
  )
  expect_no_message(
    expect_message(
      expect_message(
        expect_message(
          epidist_transform_data_model(
            prep_naive_obs,
            family = family,
            formula = formula
          ),
          "Reduced from 500 to 15 rows."
        ),
        "Data summarised by unique combinations of:"
      ),
      "Model variables"
    )
  )
})
