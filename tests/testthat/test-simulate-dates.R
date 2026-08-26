# fmt: skip file
test_that("simulate_dates returns dates offset from the outbreak start", {
  data <- data.frame(ptime = c(0, 1.5, 10.9), stime = c(2.2, 4.0, 12.1))
  start <- as.Date("2024-02-01")

  out <- simulate_dates(data, outbreak_start_date = start)

  expect_s3_class(out$pdate_lwr, "Date")
  expect_s3_class(out$sdate_lwr, "Date")
  # Times are floored, so an event at 1.5 falls on the same date as one at 1.
  expect_identical(out$pdate_lwr, start + c(0, 1, 10))
  expect_identical(out$sdate_lwr, start + c(2, 4, 12))
  # Each window is one day wide.
  expect_identical(out$pdate_upr, out$pdate_lwr + 1)
  expect_identical(out$sdate_upr, out$sdate_lwr + 1)
})

test_that("simulate_dates drops the underlying times unless asked", {
  data <- data.frame(ptime = 1, stime = 2, delay = 1)

  times <- c("ptime", "stime", "delay")
  expect_false(any(times %in% names(simulate_dates(data))))
  kept <- simulate_dates(data, keep_times = TRUE)
  expect_true(all(times %in% names(kept)))
})

test_that("simulate_dates adds an observation date only when asked", {
  data <- data.frame(ptime = 1, stime = 2)

  expect_false("obs_date" %in% names(simulate_dates(data)))
  out <- simulate_dates(
    data,
    outbreak_start_date = as.Date("2024-02-01"),
    obs_time = 60
  )
  expect_identical(out$obs_date, as.Date("2024-04-01"))
})

test_that("simulate_dates rejects bad input", {
  data <- data.frame(ptime = 1, stime = 2)

  expect_error(simulate_dates(data.frame(x = 1)), "names")
  expect_error(
    simulate_dates(data, outbreak_start_date = "2024-02-01"),
    "Must be of class 'Date'"
  )
  expect_error(simulate_dates(data, obs_time = -1), "not >= 0")
})

test_that("simulate_dates output can be used by as_epidist_linelist_data", {
  # The column names are chosen to match, so this should need no renaming.
  data <- simulate_gillespie(seed = 1) |>
    simulate_secondary(meanlog = 1.8, sdlog = 0.5) |>
    simulate_dates(outbreak_start_date = as.Date("2024-02-01"), obs_time = 60)

  linelist <- suppressMessages(as_epidist_linelist_data(
    data,
    pdate_lwr = "pdate_lwr",
    pdate_upr = "pdate_upr",
    sdate_lwr = "sdate_lwr",
    sdate_upr = "sdate_upr",
    obs_date = "obs_date"
  ))

  expect_s3_class(linelist, "epidist_linelist_data")
  expect_true(all(linelist$stime_lwr >= linelist$ptime_lwr))
})
