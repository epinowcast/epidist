# fmt: skip file
test_that("as_epidist_latent_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_model")
})

test_that("as_epidist_latent_model.epidist_linelist_data errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_latent_model(list()))
  expect_error(as_epidist_latent_model(suppressWarnings(sim_obs[, 1])))
})

test_that("as_epidist_latent_model.epidist_aggregate_data works correctly", {
  # Create test aggregate data
  agg_data <- sierra_leone_ebola_data |>
    dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
    as_epidist_aggregate_data(
      pdate_lwr = "date_of_symptom_onset",
      sdate_lwr = "date_of_sample_tested"
    ) |>
    suppressMessages()

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
    agg_data <- sierra_leone_ebola_data |>
      dplyr::count(
        date_of_symptom_onset, date_of_sample_tested, age
      ) |>
      as_epidist_aggregate_data(
        pdate_lwr = "date_of_symptom_onset",
        sdate_lwr = "date_of_sample_tested",
        by = "age_group"
      ) |>
      suppressMessages()

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
  expect_error(assert_epidist(suppressWarnings(prep_obs[, 1])))
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

test_that("as_epidist_latent_model defaults to a uniform primary event", {
  model <- as_epidist_latent_model(sim_obs)
  expect_identical(attr(model, "primary"), "uniform")
  expect_false("pgrowth" %in% epidist_family(model)$dpars)
})

test_that("an expgrowth primary event adds a pgrowth parameter", {
  model <- as_epidist_latent_model(sim_obs, primary = "expgrowth")
  expect_identical(attr(model, "primary"), "expgrowth")
  expect_true("pgrowth" %in% epidist_family(model)$dpars)

  code <- as.character(epidist(model, fn = brms::make_stancode))
  # The rate has to be estimated, not fixed.
  expect_match(code, "b_pgrowth", fixed = TRUE)
  # expgrowth_lpdf normalises, which matters because pgrowth is estimated.
  expect_match(code, "real expgrowth_lpdf", fixed = TRUE)
  # The generic primary dispatch is used, selecting expgrowth by its id.
  expect_match(code, "dot_primary_raw_lpdf", fixed = TRUE)
  expect_match(code, "real primary_lpdf", fixed = TRUE)
})

test_that("the growth rate takes a formula", {
  data <- sim_obs
  data$sex <- rep_len(c(0, 1), nrow(data))
  model <- as_epidist_latent_model(data, primary = "expgrowth")

  code <- as.character(epidist(
    model,
    formula = brms::bf(mu ~ 1, pgrowth ~ 1 + sex),
    fn = brms::make_stancode
  ))
  # A design matrix for pgrowth means the rate can vary by covariate.
  expect_match(code, "X_pgrowth", fixed = TRUE)
})

test_that("a uniform primary event adds no tilt to the likelihood", {
  code <- as.character(
    epidist(as_epidist_latent_model(sim_obs), fn = brms::make_stancode)
  )
  expect_no_match(code, "expgrowth", fixed = TRUE)
})
