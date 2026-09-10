test_that("simulate_secondary draws delays from a dist_spec", {
  set.seed(1)
  data <- data.frame(case = 1:500, ptime = seq(0, 50, length.out = 500))

  out <- simulate_secondary(
    data,
    distspec::LogNormal(meanlog = 1.8, sdlog = 0.5)
  )

  expect_named(out, c("case", "ptime", "delay", "stime"))
  expect_type(out$delay, "double")
  expect_true(all(out$delay > 0))
  expect_identical(out$stime, out$ptime + out$delay)
  expect_equal(mean(log(out$delay)), 1.8, tolerance = 0.1)
  expect_equal(stats::sd(log(out$delay)), 0.5, tolerance = 0.1)
})

test_that("simulate_secondary draws from each supported family", {
  set.seed(1)
  data <- data.frame(ptime = rep(0, 1000))

  gamma_delay <- simulate_secondary(
    data, distspec::Gamma(shape = 2, rate = 0.5)
  )$delay
  weibull_delay <- simulate_secondary(
    data, distspec::Weibull(shape = 2, scale = 7)
  )$delay

  expect_equal(mean(gamma_delay), 4, tolerance = 0.2)
  expect_equal(mean(weibull_delay), 7 * gamma(1.5), tolerance = 0.2)
})

test_that("simulate_secondary resolves uncertain parameters per row", {
  set.seed(1)
  data <- data.frame(ptime = rep(0, 2000))
  fixed <- distspec::LogNormal(meanlog = 1.8, sdlog = 0.5)
  uncertain <- distspec::LogNormal(
    meanlog = distspec::Normal(mean = 1.8, sd = 0.5),
    sdlog = 0.5
  )

  fixed_delay <- simulate_secondary(data, fixed)$delay
  uncertain_delay <- simulate_secondary(data, uncertain)$delay

  # The uncertainty in meanlog widens the delays but leaves the centre alone.
  expect_gt(stats::sd(log(uncertain_delay)), stats::sd(log(fixed_delay)))
  expect_equal(mean(log(uncertain_delay)), 1.8, tolerance = 0.1)
  expect_equal(
    stats::sd(log(uncertain_delay)),
    sqrt(0.5^2 + 0.5^2),
    tolerance = 0.1
  )
})

test_that("simulate_secondary keeps the columns and grouping of its input", {
  set.seed(1)
  data <- dplyr::group_by(
    data.frame(
      ptime = c(0, 1, 2, 3),
      location = c("a", "a", "b", "b"),
      stringsAsFactors = FALSE
    ),
    location
  )

  out <- simulate_secondary(data, distspec::LogNormal(1.8, 0.5))

  expect_identical(dplyr::group_vars(out), "location")
  expect_identical(out$location, data$location)
  expect_identical(nrow(out), 4L)
})

test_that("simulate_secondary handles a dist_spec with no draws to make", {
  data <- data.frame(ptime = numeric(0))

  out <- simulate_secondary(data, distspec::LogNormal(1.8, 0.5))

  expect_identical(nrow(out), 0L)
  expect_identical(out$delay, numeric(0))
})

test_that("simulate_secondary points a function at the distspec equivalent", {
  data <- data.frame(ptime = 0)

  expect_error(
    simulate_secondary(data, rlnorm),
    "must be a <dist_spec>, not a function"
  )
  expect_error(
    simulate_secondary(data, rlnorm),
    "distspec::LogNormal"
  )
})

test_that("simulate_secondary only accepts a single dist_spec", {
  data <- data.frame(ptime = 0)

  expect_error(
    simulate_secondary(data, list(meanlog = 1.8)),
    "dist_spec"
  )
  expect_error(
    simulate_secondary(
      data,
      c(distspec::LogNormal(1.8, 0.5), distspec::LogNormal(1.5, 0.5))
    ),
    "single delay distribution, not 2"
  )
})
