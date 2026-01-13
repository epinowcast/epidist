# fmt: skip file
test_that("as_epidist_marginal_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_marginal_obs <- as_epidist_marginal_model(sim_obs)
  expect_s3_class(prep_marginal_obs, "data.frame")
  expect_s3_class(prep_marginal_obs, "epidist_marginal_model")
})

test_that("as_epidist_marginal_model.epidist_linelist_data errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_epidist_marginal_model(list()))
  expect_error(as_epidist_marginal_model(sim_obs[, 1]))
})

test_that("as_epidist_marginal_model.epidist_linelist_data respects weight variable", { # nolint: line_length_linter.
  # Create test data with a weight column
  weighted_data <- sim_obs
  weighted_data$counts <- rep_len(c(1, 2), weighted_data)

  # Check weighted model has correct n values
  weighted_model <- as_epidist_marginal_model(
    weighted_data,
    weight = "counts"
  )
  expect_identical(weighted_model$n, weighted_data$counts)

  # Check unweighted model has n=1
  unweighted_model <- as_epidist_marginal_model(sim_obs)
  expect_true(all(unweighted_model$n == 1))
})

test_that(
  "as_epidist_marginal_model.epidist_linelist_data errors with invalid weight column", # nolint: line_length_linter.
  {
    expect_error(
      as_epidist_marginal_model(sim_obs, weight = "nonexistent_column"),
      regexp = "Names must include the elements"
    )
  }
)

test_that(
  "as_epidist_marginal_model.epidist_linelist_data handles obs_time_threshold correctly", # nolint
  {
    # Create test data with some large observation times
    test_data <- suppressMessages(sierra_leone_ebola_data |> # nolint
      dplyr::filter(date_of_sample_tested < as.Date("2015-01-01")) |> # nolint
      as_epidist_linelist_data( 
        pdate_lwr = "date_of_symptom_onset",
        sdate_lwr = "date_of_sample_tested"
      ))

    # Check threshold behaviour with default threshold = 2
    expect_message(
      model_default <- as_epidist_marginal_model(test_data), # nolint
      "Setting"
    )
    expect_true(any(is.infinite(model_default$relative_obs_time)))
    expect_identical(sum(is.infinite(model_default$relative_obs_time)), 2008L)

    # Check threshold behaviour with custom threshold = 4
    expect_message(
      model_custom <- as_epidist_marginal_model( # nolint
        test_data,
        obs_time_threshold = 1
      ),
      "Setting"
    )
    expect_true(any(is.infinite(model_custom$relative_obs_time)))
    expect_identical(sum(is.infinite(model_custom$relative_obs_time)), 4949L)

    # Check threshold behaviour with no observations beyond threshold
    expect_no_message(
      model_none <- as_epidist_marginal_model( # nolint
        test_data,
        obs_time_threshold = 100
      )
    )
    expect_false(any(is.infinite(model_none$relative_obs_time)))
  }
)


test_that(
  "as_epidist_marginal_model.epidist_aggregate_data works with aggregate data",
  {
    # Check no error when creating marginal model from aggregate data
    expect_no_error(
      marginal_agg <- as_epidist_marginal_model(agg_sim_obs) # nolint
    )

    # Check classes
    expect_s3_class(marginal_agg, "data.frame")
    expect_s3_class(marginal_agg, "epidist_marginal_model")
    expect_s3_class(marginal_agg, "epidist_aggregate_data")
    expect_s3_class(marginal_agg, "epidist_linelist_data")
    # Check passes assert_epidist for each class using S3 dispatch
    expect_no_error(assert_epidist.epidist_marginal_model(marginal_agg))
    expect_no_error(assert_epidist.epidist_aggregate_data(marginal_agg))
    expect_no_error(assert_epidist.epidist_linelist_data(marginal_agg))

    # Check n values preserved
    expect_identical(marginal_agg$n, agg_sim_obs$n)
  }
)

test_that(
  "as_epidist_marginal_model.epidist_aggregate_data preserves stratification",
  {
    # Create marginal model from stratified aggregate data
    marginal_agg_sex <- as_epidist_marginal_model(agg_sim_obs_sex)

    # Check sex column preserved
    expect_true("sex" %in% names(marginal_agg_sex))

    # Check sex values match original data
    expect_identical(
      sort(unique(marginal_agg_sex$sex)),
      sort(unique(agg_sim_obs_sex$sex))
    )

    # Check n values preserved
    expect_identical(marginal_agg_sex$n, agg_sim_obs_sex$n)
  }
)

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

test_that(
  "assert_epidist.epidist_marginal_model errors when delay_upr != delay_lwr + swindow", # nolint
  {
    bad_data <- prep_marginal_obs
    bad_data$delay_upr <- bad_data$delay_lwr + bad_data$swindow + 1
    expect_error(
      assert_epidist(bad_data),
      "delay_upr must equal delay_lwr \\+ swindow" # nolint
    )
  }
)

test_that(
  "assert_epidist.epidist_marginal_model errors when relative_obs_time < delay_upr", # nolint
  {
    bad_data <- prep_marginal_obs
    bad_data$relative_obs_time <- bad_data$delay_upr - 1
    expect_error(
      assert_epidist(bad_data),
      "relative_obs_time must be greater than or equal to delay_upr"
    )
  }
)


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
