test_that("plot_events returns a ggplot with the event windows", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs)
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2]]$geom, "GeomSegment")
  # The default draws every case up to the limit, in primary event order
  expect_identical(nrow(p$data), min(nrow(sim_obs), 200L))
  expect_false(is.unsorted(p$data$primary_lwr))
  expect_identical(p$labels$x, "Event time")
  expect_identical(p$labels$colour, "Event")
})

test_that("plot_events limits the number of cases drawn", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs, n = 50)
  expect_identical(nrow(p$data), 50L)
  # The cases kept are spread evenly over the primary event order
  expect_identical(p$data$case[1], 1L)
  expect_identical(p$data$case[50], nrow(sim_obs))
  expect_identical(nrow(p$layers[[2]]$data), 100L)
  p_all <- plot_events(sim_obs, n = Inf)
  expect_identical(nrow(p_all$data), nrow(sim_obs))
})

test_that("plot_events adds an observation time line and a grouping colour", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs_sex, obs_time = 20, by = "sex")
  expect_length(p$layers, 3)
  expect_s3_class(p$layers[[3]]$geom, "GeomVline")
  expect_identical(p$layers[[3]]$data$xintercept, 20)
  expect_identical(p$labels$colour, "sex")
  expect_s3_class(p$layers[[2]]$data$sex, "factor")
})

test_that("plot_events uses dates when the data has them", {
  skip_if_not_installed("ggplot2")
  linelist <- suppressMessages(as_epidist_linelist_data(
    sierra_leone_ebola_data,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ))
  p <- plot_events(linelist, obs_time = as.Date("2014-10-01"), n = 20)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$data$primary_lwr, "Date")
  expect_identical(p$labels$x, "Event date")
  expect_error(plot_events(linelist, obs_time = 10), "must be a date")
})

test_that("plot_events errors on bad input", {
  skip_if_not_installed("ggplot2")
  expect_error(plot_events(data.frame(x = 1)), "epidist_linelist_data")
  expect_error(plot_events(sim_obs, by = "missing"), "missing")
  expect_error(plot_events(sim_obs, by = 1), "string")
  expect_error(plot_events(sim_obs, n = 0), "not >= 1")
  expect_error(plot_events(sim_obs, obs_time = "a"), "obs_time")
})
