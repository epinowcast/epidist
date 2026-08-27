# fmt: skip file
test_that("draws_to_multivariate reports column means and covariance", {
  set.seed(11)
  draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
  reported <- draws_to_multivariate(draws, study = "A", n = 200)
  expect_identical(reported$data$study, rep("A", 2))
  expect_identical(reported$data$type, c("mean", "sd"))
  expect_identical(reported$data$value, unname(colMeans(draws)))
  expect_identical(reported$data$p, rep(NA_real_, 2))
  expect_identical(reported$data$n, rep(200, 2))
  expect_named(reported$vcov, "A")
  expect_identical(reported$vcov$A, unname(stats::cov(draws)))
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data, vcov = reported$vcov
  )))
})

test_that("draws_to_multivariate takes summary types from column names", {
  draws <- cbind(sd = rnorm(50, 3, 0.1), mean = rnorm(50, 7, 0.1))
  reported <- draws_to_multivariate(draws, study = "A", n = 100)
  expect_identical(reported$data$type, c("sd", "mean"))
})

test_that("draws_to_multivariate labels quantile columns with p", {
  draws <- matrix(rnorm(300, c(4, 7, 11), 0.1), ncol = 3, byrow = TRUE)
  reported <- draws_to_multivariate(
    draws, study = "A", type = rep("quantile", 3), p = c(0.25, 0.5, 0.75),
    n = 100
  )
  expect_identical(reported$data$p, c(0.25, 0.5, 0.75))
  mixed <- draws_to_multivariate(
    draws, study = "A", type = c("mean", "sd", "quantile"),
    p = c(NA, NA, 0.9), n = 100
  )
  expect_identical(mixed$data$p, c(NA, NA, 0.9))
})

test_that("draws_to_multivariate accepts a data frame of draws", {
  draws <- data.frame(mean = rnorm(50, 7, 0.1), sd = rnorm(50, 3, 0.1))
  reported <- draws_to_multivariate(draws, study = "A", n = 100)
  expect_identical(reported$data$type, c("mean", "sd"))
  expect_identical(dim(reported$vcov$A), c(2L, 2L))
})

test_that("draws_to_multivariate reports a supplied value", {
  draws <- cbind(mean = rnorm(50, 7, 0.1), sd = rnorm(50, 3, 0.1))
  reported <- draws_to_multivariate(
    draws, study = "A", value = c(7.1, 3.2), n = 100
  )
  expect_identical(reported$data$value, c(7.1, 3.2))
})

test_that("draws_to_multivariate rejects draws it cannot summarise", {
  draws <- cbind(mean = rnorm(50), sd = rnorm(50))
  expect_error(
    draws_to_multivariate(draws[1:2, ], study = "A"),
    "covariance matrix is singular"
  )
  expect_error(
    draws_to_multivariate(cbind(a = rnorm(50), b = rnorm(50)), study = "A"),
    "must say what each column"
  )
  expect_error(
    draws_to_multivariate(draws, study = "A", type = c("mean", "median")),
    "type"
  )
  expect_error(
    draws_to_multivariate(
      draws, study = "A", type = c("mean", "quantile")
    ),
    "must give the probability"
  )
  expect_error(
    draws_to_multivariate(
      draws, study = "A", type = rep("quantile", 2), p = c(0.1, 0.5, 0.9)
    ),
    "one entry per quantile column"
  )
  expect_error(
    draws_to_multivariate(cbind(mean = rnorm(50), sd = 3), study = "A"),
    "not positive definite"
  )
  expect_error(
    draws_to_multivariate(
      data.frame(mean = rnorm(50), sd = "a", stringsAsFactors = FALSE),
      study = "A"
    ),
    "must be numeric"
  )
})

test_that("delays_to_multivariate returns summaries with their covariance", {
  set.seed(11)
  delays <- rlnorm(300, 1.6, 0.5)
  reported <- delays_to_multivariate(
    delays,
    study = "A", probs = c(0.25, 0.75), n_bootstrap = 400,
    cens_adjusted = 1
  )
  expect_identical(reported$data$type, c("mean", "sd", "quantile", "quantile"))
  expect_identical(reported$data$p, c(NA, NA, 0.25, 0.75))
  expect_identical(reported$data$value[1], mean(delays))
  expect_identical(reported$data$value[2], stats::sd(delays))
  expect_true(all(reported$data$cens_adjusted == 1))
  expect_identical(reported$data$n, rep(length(delays), 4L))
  expect_named(reported$vcov, "A")
  expect_identical(dim(reported$vcov$A), c(4L, 4L))
  # A sample mean has a variance of about sigma squared over n.
  expect_equal(
    reported$vcov$A[1, 1], stats::var(delays) / length(delays),
    tolerance = 0.25
  )
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data,
    vcov = reported$vcov
  )))
})

test_that("delays_to_multivariate rejects a rank deficient bootstrap", {
  expect_error(
    delays_to_multivariate(rlnorm(50), study = "A", n_bootstrap = 2),
    "must exceed the 2 summaries"
  )
  expect_error(
    delays_to_multivariate(
      rlnorm(50),
      study = "A", moments = character(0)
    ),
    "at least one of"
  )
})

test_that("parameters_to_multivariate converts a reported fit", {
  reported <- parameters_to_multivariate(
    "lognormal", c(meanlog = 1.6, sdlog = 0.5),
    study = "A", se = c(0.03, 0.02), cens_adjusted = 1
  )
  expect_identical(reported$data$type, c("mean", "sd"))
  expect_equal(reported$data$value[1], exp(1.6 + 0.5^2 / 2), tolerance = 1e-8)
  expect_equal(
    reported$data$value[2],
    exp(1.6 + 0.5^2 / 2) * sqrt(expm1(0.5^2)),
    tolerance = 1e-8
  )
  expect_named(reported$vcov, "A")
  expect_identical(dim(reported$vcov$A), c(2L, 2L))
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data, vcov = reported$vcov
  )))
})

test_that("parameters_to_multivariate accepts both gamma parameterisations", {
  by_scale <- parameters_to_multivariate(
    "gamma", c(shape = 4, scale = 2), study = "A", n = 100
  )
  by_rate <- parameters_to_multivariate(
    "gamma", c(shape = 4, rate = 0.5), study = "A", n = 100
  )
  expect_equal(by_scale$data$value, by_rate$data$value, tolerance = 1e-10)
  expect_equal(by_scale$data$value, c(8, 4), tolerance = 1e-8)
})

test_that("parameters_to_multivariate reports quantiles of the fitted family", {
  reported <- parameters_to_multivariate(
    "weibull", c(shape = 2, scale = 9),
    study = "A", moments = character(0), probs = c(0.5, 0.9),
    se = c(0.1, 0.4)
  )
  expect_identical(reported$data$type, rep("quantile", 2))
  expect_identical(reported$data$p, c(0.5, 0.9))
  expect_equal(
    reported$data$value, stats::qweibull(c(0.5, 0.9), 2, 9), tolerance = 1e-8
  )
})

test_that("parameters_to_multivariate matches a Monte Carlo delta method", {
  skip_if_not_installed("MASS")
  set.seed(12)
  parameter_vcov <- diag(c(0.03, 0.02)^2)
  reported <- parameters_to_multivariate(
    "lognormal", c(meanlog = 1.6, sdlog = 0.5),
    study = "A", vcov = parameter_vcov
  )
  draws <- MASS::mvrnorm(200000, c(1.6, 0.5), parameter_vcov)
  simulated <- cbind(
    exp(draws[, 1] + draws[, 2]^2 / 2),
    exp(draws[, 1] + draws[, 2]^2 / 2) * sqrt(expm1(draws[, 2]^2))
  )
  expect_equal(reported$vcov$A, stats::cov(simulated), tolerance = 0.05)
})

test_that("parameters_to_multivariate returns no covariance without se", {
  reported <- parameters_to_multivariate(
    "gamma", c(shape = 4, scale = 2), study = "A", n = 100
  )
  expect_null(reported$vcov)
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data, vcov = reported$vcov
  )))
})

test_that("parameters_to_multivariate rejects input it cannot convert", {
  expect_error(
    parameters_to_multivariate("burr", c(shape = 1), study = "A"),
    "family"
  )
  expect_error(
    parameters_to_multivariate("gamma", c(mean = 4, sd = 2), study = "A"),
    "must report"
  )
  expect_error(
    parameters_to_multivariate("gamma", c(shape = -1, scale = 2), study = "A"),
    "greater than zero"
  )
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A",
      se = c(0.1, 0.1), vcov = diag(2)
    ),
    "at most one of"
  )
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A", vcov = matrix(1, 3, 3)
    ),
    "vcov"
  )
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A",
      vcov = matrix(c(1, 2, 0, 1), 2, 2)
    ),
    "symmetric"
  )
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A",
      vcov = matrix(c(1, 2, 2, 1), 2, 2)
    ),
    "positive definite"
  )
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A",
      probs = c(0.25, 0.75), se = c(0.1, 0.1)
    ),
    "at most 2 summaries"
  )
})

test_that("parameters_to_multivariate reorders a named parameter covariance", {
  named <- matrix(
    c(0.04, 0.005, 0.005, 0.0025), 2, 2,
    dimnames = list(c("scale", "shape"), c("scale", "shape"))
  )
  reordered <- parameters_to_multivariate(
    "gamma", c(shape = 4, scale = 2), study = "A", vcov = named
  )
  direct <- parameters_to_multivariate(
    "gamma", c(shape = 4, scale = 2), study = "A",
    vcov = matrix(c(0.0025, 0.005, 0.005, 0.04), 2, 2)
  )
  expect_equal(reordered$vcov$A, direct$vcov$A, tolerance = 1e-10)
  expect_error(
    parameters_to_multivariate(
      "gamma", c(shape = 4, scale = 2), study = "A",
      vcov = matrix(
        1, 2, 2, dimnames = list(c("a", "b"), c("a", "b"))
      )
    ),
    "is named for"
  )
})

test_that("a reported fit implies the summaries its biased procedure gave", {
  # A study fits a lognormal to naive integer date differences taken from a
  # right truncated cohort. Its reported parameters describe the biased
  # distribution its procedure converged to, so the summaries they imply must
  # match the summaries the meta model forward models for that design.
  set.seed(13)
  meanlog <- 1.6
  sdlog <- 0.5
  obs_time <- 20
  ptime <- stats::runif(2e5, 0, 1)
  delay <- stats::rlnorm(2e5, meanlog, sdlog)
  observed <- floor(ptime + delay)
  # A study fitting on the log scale cannot use a delay of zero, so it counted
  # only delays of a day or more, which is a left truncation at one.
  observed <- observed[observed + 1 <= obs_time & observed > 0]
  reported_log <- c(
    meanlog = mean(log(observed)), sdlog = stats::sd(log(observed))
  )
  reported <- parameters_to_multivariate(
    "lognormal", reported_log,
    study = "A", se = c(0.01, 0.01),
    relative_obs_time = obs_time, trunc_adjusted = FALSE, cens_adjusted = 0,
    delay_min = 1
  )
  implied <- .meta_implied_moments(
    "plnorm", list(meanlog = meanlog, sdlog = sdlog),
    lower = 1, cutoff = obs_time, pwindow = 1, swindow = 1,
    trunc_adjusted = 0, cens_adjusted = 0, growth_rate = 0
  )
  expect_equal(
    reported$data$value,
    c(implied[["mean"]], implied[["sd"]]),
    tolerance = 0.02
  )
  # Without the conditioning the reported standard deviation carries the tail
  # of a lognormal that runs past the study's observation time, and so
  # overstates the spread of the delays the study saw.
  unconditioned <- parameters_to_multivariate(
    "lognormal", reported_log,
    study = "A", se = c(0.01, 0.01)
  )
  expect_gt(unconditioned$data$value[2], reported$data$value[2])
  expect_gt(
    abs(unconditioned$data$value[2] - implied[["sd"]]),
    abs(reported$data$value[2] - implied[["sd"]])
  )
})

test_that("draws_to_multivariate works on predict_delay_parameters output", {
  # The shape predict_delay_parameters() returns for a lognormal fit.
  set.seed(14)
  dpars <- data.frame(
    draw = seq_len(500), index = 1L,
    mu = rnorm(500, 1.6, 0.03), sigma = rnorm(500, 0.5, 0.02)
  )
  class(dpars) <- c("lognormal_samples", class(dpars))
  dpars <- add_mean_sd(dpars)
  reported <- draws_to_multivariate(
    dpars[, c("mean", "sd")],
    study = "A", n = 200, cens_adjusted = 1
  )
  expect_identical(reported$data$type, c("mean", "sd"))
  expect_equal(
    reported$data$value, unname(colMeans(dpars[, c("mean", "sd")])),
    tolerance = 1e-10
  )
  expect_gt(reported$vcov$A[1, 2], 0)
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data, vcov = reported$vcov
  )))
})
