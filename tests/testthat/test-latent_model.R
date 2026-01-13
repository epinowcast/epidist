# fmt: skip file
test_that("as_epidist_latent_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_model")
})

test_that("as_epidist_latent_model.epidist_linelist_data errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_latent_model(list()))
  expect_error(as_epidist_latent_model(sim_obs[, 1]))
})

test_that("as_epidist_latent_model.epidist_aggregate_data works correctly", {
  # Create test aggregate data
  agg_data <- suppressMessages(sierra_leone_ebola_data |> # nolint
    dplyr::count(date_of_symptom_onset, date_of_sample_tested) |> # nolint
    as_epidist_aggregate_data(
      pdate_lwr = "date_of_symptom_onset",
      sdate_lwr = "date_of_sample_tested"
    ))

  # Convert to latent model format
  latent_data <- as_epidist_latent_model(agg_data)

  # Check classes
  expect_s3_class(latent_data, "data.frame")
  expect_s3_class(latent_data, "epidist_latent_model")

  # Check number of rows matches sum of counts
  expect_identical(nrow(latent_data), sum(agg_data$n))
  expect_identical(nrow(latent_data), nrow(sierra_leone_ebola_data))

  # Check that n has been removed
  expect_false("n" %in% names(latent_data))

  # Check required columns present
  expect_true(all(.linelist_required_cols() %in% names(latent_data)))
})

test_that(
  "as_epidist_latent_model.epidist_aggregate_data preserves stratification",
  {
    # Create test aggregate data with stratification
    agg_data <- suppressMessages(sierra_leone_ebola_data |> # nolint
      dplyr::count(
        date_of_symptom_onset, date_of_sample_tested, age
      ) |>
      as_epidist_aggregate_data(
        pdate_lwr = "date_of_symptom_onset",
        sdate_lwr = "date_of_sample_tested",
        by = "age_group"
      ))

    # Convert to latent model
    latent_data <- as_epidist_latent_model(agg_data)

    # Check stratification variable preserved
    expect_true("age" %in% names(latent_data))

    # Check counts match between stratified groups
    expect_identical(
      as.double(sort(table(latent_data$age))),
      as.double(sort(tapply(agg_data$n, agg_data$age, sum)))
    )

    # Check number of rows matches sum of counts
    expect_identical(nrow(latent_data), sum(agg_data$n))
    expect_identical(nrow(latent_data), nrow(sierra_leone_ebola_data))

    # Check that n has been removed
    expect_false("n" %in% names(latent_data))

    # Check required columns present
    expect_true(all(.linelist_required_cols() %in% names(latent_data)))
  }
)

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
