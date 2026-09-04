# fmt: skip file
test_that("epidist_estimates_summaries builds one study's rows", {
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_summaries(
    "A",
    mean = 7.5, sd = 3.6, quantiles = c(4.2, 9.4),
    probs = c(0.25, 0.75), n = 120, cens_adjusted = 1
  )))
  expect_true(is_epidist_estimates_data(estimates))
  expect_identical(estimates$type, c("mean", "sd", "quantile", "quantile"))
  expect_identical(estimates$value, c(7.5, 3.6, 4.2, 9.4))
  expect_identical(estimates$p, c(NA, NA, 0.25, 0.75))
  expect_identical(estimates$n, rep(120, 4))
  expect_true(all(estimates$cens_adjusted == 1L))
})

test_that("epidist_estimates_summaries takes a standard error per summary", {
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_summaries(
    "A",
    mean = 7.5, sd = 3.6, se = c(0.4, 0.3)
  )))
  expect_identical(estimates$se, c(0.4, 0.3))
  expect_error(
    epidist_estimates_summaries("A", mean = 7.5, se = c(0.4, 0.3)),
    "se"
  )
  expect_error(
    epidist_estimates_summaries("A", n = 100),
    "at least one of"
  )
  expect_error(
    epidist_estimates_summaries("A", quantiles = c(4, 9), n = 100),
    "probs"
  )
})

test_that("epidist_estimates_parameters converts a reported fit", {
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "lognormal", c(meanlog = 1.6, sdlog = 0.5),
    se = c(0.03, 0.02), cens_adjusted = 1
  )))
  expect_identical(estimates$type, c("mean", "sd"))
  expect_equal(estimates$value[1], exp(1.6 + 0.5^2 / 2), tolerance = 1e-8)
  expect_equal(
    estimates$value[2],
    exp(1.6 + 0.5^2 / 2) * sqrt(expm1(0.5^2)),
    tolerance = 1e-8
  )
  # Reported parameter standard errors reach the fit as the delta method
  # covariance over the summaries, so the rows carry no standard error of
  # their own and are bound by a covariance matrix instead.
  expect_true(all(is.na(estimates$se)))
  expect_identical(estimates$mvn_id, c("A", "A"))
  covariance <- .estimates_vcov(estimates)[["A"]]
  expect_identical(dim(covariance), c(2L, 2L))
  expect_true(all(diag(covariance) > 0))
  expect_gt(covariance[1, 2], 0)
})

test_that("epidist_estimates_parameters carries the parameter information exactly", { # nolint: line_length_linter.
  # A study that fitted the same family as the model and adjusted for both
  # biases carries the information V^-1 about its parameters. Fitting its k
  # summaries with the k by k delta method covariance J V J^T gives the
  # curvature J^T (J V J^T)^-1 J = V^-1 at the reported parameters, so the
  # Hessian standard errors of the row match the reported standard errors.
  meanlog <- 1.6
  sdlog <- 0.5
  n <- 1000
  se <- c(sdlog / sqrt(n), sdlog / sqrt(2 * n))
  estimates <- suppressMessages(epidist_estimates_parameters(
    "A", "lognormal", c(meanlog = meanlog, sdlog = sdlog),
    se = se, n = n, cens_adjusted = 1, trunc_adjusted = TRUE
  ))
  prep <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(prep$obs_type, 7L)
  standata <- suppressMessages(epidist(prep, fn = brms::make_standata))
  slots <- .meta_row_slots(1, list(data = standata))
  negative_ll <- function(theta) {
    return(-.meta_row_log_lik(
      slots, "plnorm", list(meanlog = theta[1], sdlog = theta[2])
    ))
  }
  step <- 1e-4
  truth <- c(meanlog, sdlog)
  hessian <- matrix(0, 2, 2)
  for (i in 1:2) {
    for (j in 1:2) {
      e_i <- replace(c(0, 0), i, step)
      e_j <- replace(c(0, 0), j, step)
      hessian[i, j] <- (
        negative_ll(truth + e_i + e_j) - negative_ll(truth + e_i - e_j) -
          negative_ll(truth - e_i + e_j) + negative_ll(truth - e_i - e_j)
      ) / (4 * step^2)
    }
  }
  expect_equal(sqrt(diag(solve(hessian))), se, tolerance = 0.02)
  expect_lt(abs(hessian[1, 2]) / sqrt(hessian[1, 1] * hessian[2, 2]), 0.05)
})

test_that("epidist_estimates_parameters accepts both gamma parameterisations", {
  by_scale <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "gamma", c(shape = 4, scale = 2),
    n = 100
  )))
  by_rate <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "gamma", c(shape = 4, rate = 0.5),
    n = 100
  )))
  expect_equal(by_scale$value, by_rate$value, tolerance = 1e-10)
  expect_equal(by_scale$value, c(8, 4), tolerance = 1e-8)
})

test_that("epidist_estimates_parameters reports quantiles of the fit", {
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "weibull", c(shape = 2, scale = 9),
    moments = character(0), probs = c(0.5, 0.9), se = c(0.1, 0.4)
  )))
  expect_identical(estimates$type, rep("quantile", 2))
  expect_identical(estimates$p, c(0.5, 0.9))
  expect_equal(
    estimates$value, stats::qweibull(c(0.5, 0.9), 2, 9),
    tolerance = 1e-8
  )
})

test_that("epidist_estimates_parameters matches a Monte Carlo delta method", {
  skip_if_not_installed("MASS")
  set.seed(12)
  parameter_se <- c(0.03, 0.02)
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "lognormal", c(meanlog = 1.6, sdlog = 0.5),
    se = parameter_se
  )))
  draws <- MASS::mvrnorm(200000, c(1.6, 0.5), diag(parameter_se^2))
  simulated <- cbind(
    exp(draws[, 1] + draws[, 2]^2 / 2),
    exp(draws[, 1] + draws[, 2]^2 / 2) * sqrt(expm1(draws[, 2]^2))
  )
  expect_equal(
    .estimates_vcov(estimates)[["A"]], stats::cov(simulated),
    tolerance = 0.05
  )
})

test_that("epidist_estimates_parameters refuses more summaries with standard errors than it has parameters", { # nolint: line_length_linter.
  # Five summaries of a two parameter fit are functions of two numbers, so
  # fitting them all would count the study's information several times over.
  expect_error(
    epidist_estimates_parameters(
      "A", "lognormal", c(meanlog = 1.6, sdlog = 0.5),
      probs = c(0.25, 0.5, 0.75), se = c(0.03, 0.02)
    ),
    "at most 2"
  )
  # Without standard errors the summaries fall back to the sample size
  # likelihoods, which carry no covariance, so any number may be reported.
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A", "lognormal", c(meanlog = 1.6, sdlog = 0.5),
    probs = c(0.25, 0.5, 0.75), n = 100
  )))
  expect_identical(nrow(estimates), 5L)
  expect_true(all(is.na(estimates$se)))
  expect_length(.estimates_vcov(estimates), 0)
})

test_that("epidist_estimates_parameters rejects input it cannot convert", {
  expect_error(
    epidist_estimates_parameters("A", "burr", c(shape = 1)),
    "family"
  )
  expect_error(
    epidist_estimates_parameters("A", "gamma", c(mean = 4, sd = 2)),
    "must report"
  )
  expect_error(
    epidist_estimates_parameters("A", "gamma", c(shape = -1, scale = 2)),
    "greater than zero"
  )
  expect_error(
    epidist_estimates_parameters(
      "A", "gamma", c(shape = 4, scale = 2),
      se = 0.1
    ),
    "se"
  )
  expect_error(
    epidist_estimates_parameters(
      "A", "gamma", c(shape = 4, scale = 2),
      moments = character(0)
    ),
    "at least one of"
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
  estimates <- suppressMessages(epidist_estimates_parameters(
    "A", "lognormal", reported_log,
    se = c(0.01, 0.01),
    relative_obs_time = obs_time, trunc_adjusted = FALSE, cens_adjusted = 0,
    delay_min = 1
  ))
  implied <- .meta_implied_moments(
    "plnorm", list(meanlog = meanlog, sdlog = sdlog),
    lower = 1, cutoff = obs_time, pwindow = 1, swindow = 1,
    trunc_adjusted = 0, cens_adjusted = 0, growth_rate = 0
  )
  expect_equal(
    estimates$value, c(implied[["mean"]], implied[["sd"]]),
    tolerance = 0.02
  )
  # Without the conditioning the reported standard deviation carries the tail
  # of a lognormal that runs past the study's observation time, and so
  # overstates the spread of the delays the study saw.
  unconditioned <- suppressWarnings(suppressMessages(
    epidist_estimates_parameters(
      "A", "lognormal", reported_log,
      se = c(0.01, 0.01)
    )
  ))
  expect_gt(unconditioned$value[2], estimates$value[2])
  expect_gt(
    abs(unconditioned$value[2] - implied[["sd"]]),
    abs(estimates$value[2] - implied[["sd"]])
  )
})

test_that("contributions from several studies combine", {
  first <- suppressWarnings(suppressMessages(epidist_estimates_summaries(
    "A",
    mean = 7.5, sd = 3.6, n = 120
  )))
  second <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "B", "gamma", c(shape = 4, scale = 2),
    n = 80
  )))
  third <- suppressMessages(epidist_estimates_summaries(
    "A",
    mean = 6.9, n = 60, relative_obs_time = 20, trunc_adjusted = FALSE
  ))
  combined <- suppressMessages(
    as_epidist_estimates_data(list(first, second, third))
  )
  expect_identical(nrow(combined), 5L)
  expect_identical(unique(combined$study), c("A", "B"))
  # Combining is associative.
  nested <- suppressMessages(as_epidist_estimates_data(list(
    suppressMessages(as_epidist_estimates_data(list(first, second))), third
  )))
  expect_equal(nested, combined, ignore_attr = TRUE)
  expect_error(as_epidist_estimates_data(list()), "at least one")
})

test_that("as_epidist_estimates_data is idempotent", {
  estimates <- suppressWarnings(suppressMessages(epidist_estimates_summaries(
    "A",
    mean = 7.5, n = 120
  )))
  expect_identical(as_epidist_estimates_data(estimates), estimates)
})
