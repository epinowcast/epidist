test_that("as_epidist_aggregate_data assigns epidist_aggregate_data class to data", {
  expect_s3_class(agg_sim_obs, "epidist_aggregate_data")
})

test_that("as_epidist_aggregate_data.default works with vectors", {
  expect_no_error(
    agg_data <- as_epidist_aggregate_data(
      # nolint
      data = c(1, 2, 3),
      ptime_upr = c(2, 3, 4),
      stime_lwr = c(3, 4, 5),
      stime_upr = c(4, 5, 6),
      obs_time = c(5, 6, 7),
      n = c(1, 2, 3)
    )
  )
  expect_s3_class(agg_data, "epidist_aggregate_data")
  expect_true(all(agg_data$n >= 1))
  expect_identical(agg_data$n, c(1.0, 2.0, 3.0))
})

test_that("as_epidist_aggregate_data works with dates", {
  # Create test data with dates from sim_obs
  data <- sim_obs |>
    dplyr::mutate(
      pdate_lwr = as.Date("2023-01-01") + ptime_lwr,
      pdate_upr = as.Date("2023-01-01") + ptime_upr,
      sdate_lwr = as.Date("2023-01-01") + stime_lwr,
      sdate_upr = as.Date("2023-01-01") + stime_upr,
      obs_date = as.Date("2023-01-01") + obs_time
    ) |>
    dplyr::select(
      pdate_lwr,
      pdate_upr,
      sdate_lwr,
      sdate_upr,
      obs_date
    ) |>
    dplyr::mutate(n = 1)

  class(data) <- setdiff(class(data), "epidist_linelist_data")
  expect_no_error(
    as_epidist_aggregate_data(
      data,
      pdate_lwr = "pdate_lwr",
      pdate_upr = "pdate_upr",
      sdate_lwr = "sdate_lwr",
      sdate_upr = "sdate_upr",
      obs_date = "obs_date"
    )
  )
})

test_that("as_epidist_aggregate_data preserves additional columns", {
  # Use precalculated sex-stratified aggregate data
  expect_true("sex" %in% names(agg_sim_obs_sex))
  expect_identical(
    sort(unique(agg_sim_obs_sex$sex)),
    sort(unique(sim_obs_sex$sex))
  )
})

test_that("as_epidist_aggregate_data.epidist_linelist_data aggregates correctly", {
  # Create small test dataset with age stratification
  test_data <- sim_obs_sex |>
    dplyr::slice_head(n = 100)

  # Create aggregated versions
  agg_test <- as_epidist_aggregate_data(test_data)
  agg_test_sex <- as_epidist_aggregate_data(test_data, by = "sex")

  # Check sex column only present in stratified data
  expect_true("sex" %in% names(agg_test_sex))
  expect_false("sex" %in% names(agg_test))

  # Compare default vs age-stratified aggregation
  expect_gt(nrow(agg_test_sex), nrow(agg_test))

  # Check total counts match between stratified and unstratified
  expect_identical(sum(agg_test$n), sum(agg_test_sex$n))
  expect_identical(sum(agg_test$n), 100L)

  # Check n values are sensible
  expect_true(all(agg_test$n >= 1))
  expect_true(all(agg_test_sex$n >= 1))
})

test_that("as_epidist_aggregate_data validates counts", {
  # Test zero counts
  invalid_zero <- agg_sim_obs |>
    dplyr::mutate(n = 0)

  expect_error(
    assert_epidist(invalid_zero)
  )

  # Test non-integer counts
  invalid_decimal <- agg_sim_obs |>
    dplyr::mutate(n = 1.5)

  expect_error(
    assert_epidist(invalid_decimal)
  )
})

test_that("as_epidist_aggregate_data errors if n is missing", {
  # Test default method
  expect_error(
    as_epidist_aggregate_data(
      data = c(1, 2, 3),
      ptime_upr = c(2, 3, 4),
      stime_lwr = c(3, 4, 5),
      stime_upr = c(4, 5, 6),
      obs_time = c(5, 6, 7)
    ),
    "is NULL but must be provided"
  )

  # Test data.frame method
  test_df <- data.frame(
    pdate_lwr = as.Date("2020-01-01") + 0:2,
    sdate_lwr = as.Date("2020-01-02") + 0:2
  )

  suppressMessages(
    expect_error(
      as_epidist_aggregate_data(test_df),
      "is NULL but must be provided"
    )
  )
})

test_that("is_epidist_aggregate_data works correctly", {
  # Test positive case
  expect_true(is_epidist_aggregate_data(agg_sim_obs))

  # Test negative cases
  expect_false(is_epidist_aggregate_data(sim_obs))
  expect_false(is_epidist_aggregate_data(data.frame()))
  expect_false(is_epidist_aggregate_data(list()))
})
