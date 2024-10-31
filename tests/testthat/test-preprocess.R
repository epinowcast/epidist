test_that("as_epidist_linelist assigns epidist_linelist class to data", {
  data <- data.frame(
    case = 1,
    pdate_lwr = as.POSIXct("2023-01-01 00:00:00"),
    pdate_upr = as.POSIXct("2023-01-02 00:00:00"),
    sdate_lwr = as.POSIXct("2023-01-03 00:00:00"),
    sdate_upr = as.POSIXct("2023-01-04 00:00:00"),
    obs_date = as.POSIXct("2023-01-05 00:00:00")
  )
  linelist <- as_epidist_linelist(
    data, "pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date"
  )
  expect_s3_class(linelist, "epidist_linelist")
})

test_that("as_epidist_linelist correctly renames columns", {
  data <- data.frame(
    case = 1,
    p_lower = as.POSIXct("2023-01-01"),
    p_upper = as.POSIXct("2023-01-02"),
    s_lower = as.POSIXct("2023-01-03"),
    s_upper = as.POSIXct("2023-01-04"),
    observation = as.POSIXct("2023-01-05")
  )
  linelist <- as_epidist_linelist(
    data, "p_lower", "p_upper", "s_lower", "s_upper", "observation"
  )
  col_names <- c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date")
  expect_true(all(col_names %in% names(linelist)))
})
