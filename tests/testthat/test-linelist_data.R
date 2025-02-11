test_that(
  "as_epidist_linelist_data assigns epidist_linelist_data class to data",
  {
    data <- data.frame(
      case = 1,
      pdate_lwr = as.POSIXct("2023-01-01 00:00:00"),
      pdate_upr = as.POSIXct("2023-01-02 00:00:00"),
      sdate_lwr = as.POSIXct("2023-01-03 00:00:00"),
      sdate_upr = as.POSIXct("2023-01-04 00:00:00"),
      obs_date = as.POSIXct("2023-01-05 00:00:00")
    )
    linelist_data <- suppressMessages(as_epidist_linelist_data(
      data, "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
    ))
    expect_s3_class(linelist_data, "epidist_linelist_data")
  }
)

test_that("as_epidist_linelist_data correctly renames columns", {
  data <- data.frame(
    case = 1,
    p_lower = as.POSIXct("2023-01-01"),
    p_upper = as.POSIXct("2023-01-02"),
    s_lower = as.POSIXct("2023-01-03"),
    s_upper = as.POSIXct("2023-01-04"),
    observation = as.POSIXct("2023-01-05")
  )
  linelist_data <- suppressMessages(as_epidist_linelist_data(
    data, "p_lower", "p_upper", "s_lower", "s_upper", "observation"
  ))
  col_names <- c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date")
  expect_true(all(col_names %in% names(linelist_data)))
})

test_that("as_epidist_linelist_data works with dates", {
  data <- data.frame(
    case = 1,
    pdate_lwr = as.Date("2023-01-01"),
    pdate_upr = as.Date("2023-01-02"),
    sdate_lwr = as.Date("2023-01-03"),
    sdate_upr = as.Date("2023-01-04"),
    obs_date = as.Date("2023-01-05")
  )
  expect_no_error(
    suppressMessages(as_epidist_linelist_data(
      data, "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
    ))
  )
})

test_that("as_epidist_linelist_data works with default column names", {
  data <- data.frame(
    case = 1,
    pdate_lwr = as.Date("2023-01-01"),
    sdate_lwr = as.Date("2023-01-03")
  )
  linelist_data <- suppressMessages(as_epidist_linelist_data(data))
  expect_s3_class(linelist_data, "epidist_linelist_data")
  expect_true(
    all(
      c(
        "ptime_lwr", "ptime_upr", "stime_lwr", "stime_upr", "obs_time"
      ) %in% names(linelist_data)
    )
  )
})

test_that("as_epidist_linelist_data adds default upper bounds", {
  data <- data.frame(
    pdate_lwr = as.Date("2023-01-01"),
    sdate_lwr = as.Date("2023-01-03")
  )
  linelist_data <- suppressMessages(as_epidist_linelist_data(data))
  expect_identical(
    as.Date(linelist_data$pdate_upr),
    as.Date("2023-01-02")
  )
  expect_identical(
    as.Date(linelist_data$sdate_upr),
    as.Date("2023-01-04")
  )
})

test_that("as_epidist_linelist_data uses max secondary date as obs_date", {
  data <- data.frame(
    pdate_lwr = as.Date("2023-01-01"),
    sdate_lwr = as.Date("2023-01-03")
  )
  linelist_data <- suppressMessages(as_epidist_linelist_data(data))
  expect_identical(
    as.Date(linelist_data$obs_date),
    as.Date("2023-01-04")
  )
})

test_that("as_epidist_linelist_data errors without required columns", {
  data <- data.frame(
    case = 1,
    some_date = as.Date("2023-01-01")
  )
  expect_error(
    suppressMessages(as_epidist_linelist_data(data)),
    "`pdate_lwr` is NULL but must be provided"
  )
})

test_that("as_epidist_linelist_data preserves additional columns", {
  data <- data.frame(
    case = 1,
    pdate_lwr = as.Date("2023-01-01"),
    sdate_lwr = as.Date("2023-01-03"),
    extra_col = "test",
    stringsAsFactors = FALSE
  )
  linelist_data <- suppressMessages(as_epidist_linelist_data(data))
  expect_true("extra_col" %in% names(linelist_data))
  expect_identical(linelist_data$extra_col, "test")
})

test_that("as_epidist_linelist_data.epidist_aggregate_data works correctly", {
  # Create test aggregate data
  agg_data <- suppressMessages(sierra_leone_ebola_data |>
    dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
    as_epidist_aggregate_data(
      pdate_lwr = "date_of_symptom_onset",
      sdate_lwr = "date_of_sample_tested",
      n = "n"
    ))

  # Convert to linelist format
  linelist_data <- as_epidist_linelist_data(agg_data)

  # Check classes
  expect_s3_class(linelist_data, "data.frame")
  expect_s3_class(linelist_data, "epidist_linelist_data")

  # Check number of rows matches sum of counts
  expect_identical(nrow(linelist_data), sum(agg_data$n))
  # Check that n has been removed
  expect_false("n" %in% names(linelist_data))

  # Check other columns preserved
  expect_true(all(.linelist_required_cols() %in% names(linelist_data)))
})

test_that("is_epidist_linelist_data returns TRUE for correct input", {
  expect_true(is_epidist_linelist_data(sim_obs))
  expect_true({
    x <- list()
    class(x) <- "epidist_linelist_data"
    is_epidist_linelist_data(x)
  })
})

test_that("is_epidist_linelist_data returns FALSE for incorrect input", {
  expect_false(is_epidist_linelist_data(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_linelist_data_extension"
    is_epidist_linelist_data(x)
  })
})
