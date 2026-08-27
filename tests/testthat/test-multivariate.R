# fmt: skip file
test_that("as_epidist_multivariate reports column means and covariance", {
  set.seed(11)
  draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
  mvn <- as_epidist_multivariate(draws)
  expect_true(is_epidist_multivariate(mvn))
  expect_identical(mvn$params, c("mean", "sd"))
  expect_identical(mvn$index, 1)
  expect_identical(mvn$n_draws, 500L)
  expect_identical(mvn$value, colMeans(draws))
  expect_identical(vcov(mvn), unname(stats::cov(draws)))
  expect_output(print(mvn), "mean")
})

test_that("as_epidist_multivariate accepts a data frame of draws", {
  set.seed(11)
  draws <- data.frame(mean = rnorm(50, 7, 0.1), sd = rnorm(50, 3, 0.1))
  mvn <- as_epidist_multivariate(draws)
  expect_identical(mvn$params, c("mean", "sd"))
  expect_identical(dim(vcov(mvn)), c(2L, 2L))
})

test_that("as_epidist_multivariate takes named parameter columns", {
  set.seed(11)
  draws <- data.frame(
    draw = seq_len(50), index = 1L,
    meanlog = rnorm(50, 1.6, 0.03), sdlog = rnorm(50, 0.5, 0.02),
    label = "a", stringsAsFactors = FALSE
  )
  mvn <- as_epidist_multivariate(draws, params = c("meanlog", "sdlog"))
  expect_identical(mvn$params, c("meanlog", "sdlog"))
  expect_identical(mvn$index, 1)
  # A non numeric column is dropped by the default, which leaves the same set.
  expect_identical(as_epidist_multivariate(draws)$params, mvn$params)
})

test_that("as_epidist_multivariate orders a trajectory index major", {
  set.seed(11)
  draws <- data.frame(
    draw = rep(seq_len(200), 2),
    index = rep(c(1L, 2L), each = 200),
    mean = c(rnorm(200, 7, 0.2), rnorm(200, 9, 0.2)),
    sd = c(rnorm(200, 3, 0.1), rnorm(200, 4, 0.1))
  )
  mvn <- as_epidist_multivariate(draws)
  expect_identical(mvn$index, c(1, 2))
  expect_named(mvn$value, c("mean[1]", "sd[1]", "mean[2]", "sd[2]"))
  expect_equal(unname(mvn$value[1]), mean(draws$mean[1:200]), tolerance = 1e-10)
  expect_equal(
    unname(mvn$value[3]), mean(draws$mean[201:400]), tolerance = 1e-10
  )
  expect_identical(dim(vcov(mvn)), c(4L, 4L))
})

test_that("as_epidist_multivariate works on predict_delay_parameters output", {
  # The shape predict_delay_parameters() returns for a lognormal fit.
  set.seed(14)
  dpars <- data.frame(
    draw = seq_len(500), index = 1L,
    mu = rnorm(500, 1.6, 0.03), sigma = rnorm(500, 0.5, 0.02)
  )
  class(dpars) <- c("lognormal_samples", class(dpars))
  dpars <- add_mean_sd(dpars)
  mvn <- as_epidist_multivariate(dpars, params = c("mean", "sd"))
  expect_identical(mvn$params, c("mean", "sd"))
  expect_equal(
    unname(mvn$value), unname(colMeans(dpars[, c("mean", "sd")])),
    tolerance = 1e-10
  )
  expect_gt(vcov(mvn)[1, 2], 0)
})

test_that("as_epidist_multivariate rejects draws it cannot summarise", {
  set.seed(11)
  draws <- cbind(mean = rnorm(50), sd = rnorm(50))
  expect_error(
    as_epidist_multivariate(draws[1:2, ]),
    "covariance matrix is singular"
  )
  expect_error(
    as_epidist_multivariate(unname(draws)),
    "must name the columns"
  )
  expect_error(
    as_epidist_multivariate(cbind(mean = rnorm(50), sd = 3)),
    "has rank 1"
  )
  expect_error(
    as_epidist_multivariate(
      data.frame(mean = rnorm(50), sd = "a", stringsAsFactors = FALSE),
      params = c("mean", "sd")
    ),
    "not numeric"
  )
  expect_error(
    as_epidist_multivariate(data.frame(mean = rnorm(50)), params = "missing"),
    "no column"
  )
})

test_that("a rank deficient set of draws says so", {
  set.seed(11)
  meanlog <- rnorm(500, 1.6, 0.05)
  sdlog <- rnorm(500, 0.5, 0.02)
  # Five summaries of a two parameter fit carry two degrees of freedom.
  draws <- cbind(
    mean = exp(meanlog + sdlog^2 / 2),
    sd = exp(meanlog + sdlog^2 / 2) * sqrt(expm1(sdlog^2)),
    q0.25 = qlnorm(0.25, meanlog, sdlog),
    q0.5 = qlnorm(0.5, meanlog, sdlog),
    q0.75 = qlnorm(0.75, meanlog, sdlog)
  )
  expect_error(
    as_epidist_multivariate(draws),
    "deterministic functions of fewer underlying"
  )
})

test_that("new_epidist_multivariate takes a published mean and covariance", {
  mvn <- new_epidist_multivariate(
    value = c(mean = 7.5, sd = 3.6),
    vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
    params = c("mean", "sd")
  )
  expect_true(is_epidist_multivariate(mvn))
  expect_identical(mvn$n_draws, NA_integer_)
  expect_null(mvn$draws)
  expect_error(
    new_epidist_multivariate(
      value = c(mean = 7.5, sd = 3.6),
      vcov = matrix(c(0.09, 0.03, 0.02, 0.04), nrow = 2),
      params = c("mean", "sd")
    ),
    "symmetric"
  )
})

test_that("as_epidist_estimates_data maps multivariate elements to types", {
  set.seed(11)
  draws <- cbind(
    mean = rnorm(500, 7.5, 0.3),
    q0.5 = rnorm(500, 6.8, 0.3)
  )
  estimates <- suppressMessages(as_epidist_estimates_data(
    as_epidist_multivariate(draws), study = "A"
  ))
  expect_identical(estimates$type, c("mean", "quantile"))
  expect_identical(estimates$p, c(NA, 0.5))
  expect_identical(estimates$mvn_id, rep("A", 2))
  expect_named(.estimates_vcov(estimates), "A")
})

test_that("as_epidist_estimates_data rejects elements it cannot report", {
  set.seed(11)
  draws <- cbind(meanlog = rnorm(500, 1.6, 0.03), sdlog = rnorm(500, 0.5, 0.02))
  mvn <- as_epidist_multivariate(draws)
  expect_error(
    as_epidist_estimates_data(mvn, study = "A"),
    "not.*quantities a study reports"
  )
  expect_error(
    as_epidist_estimates_data(
      new_epidist_multivariate(
        value = c(meanlog = 1.6, sdlog = 0.5),
        vcov = diag(c(0.001, 0.0004)), params = c("meanlog", "sdlog")
      ),
      study = "A", family = "lognormal"
    ),
    "needs the draws themselves"
  )
})

test_that("the multivariate family path agrees with the delta method", {
  set.seed(11)
  draws <- cbind(
    shape = rnorm(20000, 4.1, 0.2), rate = rnorm(20000, 0.55, 0.03)
  )
  simulated <- suppressMessages(as_epidist_estimates_data(
    as_epidist_multivariate(draws),
    study = "A", family = "gamma",
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0
  ))
  linearised <- suppressMessages(epidist_estimates_parameters(
    "A", "gamma", c(shape = 4.1, rate = 0.55), se = c(0.2, 0.03),
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0
  ))
  expect_identical(simulated$type, linearised$type)
  expect_equal(simulated$value, linearised$value, tolerance = 0.02)
  expect_equal(
    sqrt(diag(.estimates_vcov(simulated)[["A"]])), linearised$se,
    tolerance = 0.05
  )
})

test_that("a multivariate trajectory cannot be fitted", {
  set.seed(11)
  draws <- data.frame(
    draw = rep(seq_len(200), 2),
    index = rep(c(1L, 2L), each = 200),
    mean = c(rnorm(200, 7, 0.2), rnorm(200, 9, 0.2)),
    sd = c(rnorm(200, 3, 0.1), rnorm(200, 4, 0.1))
  )
  expect_error(
    as_epidist_estimates_data(as_epidist_multivariate(draws), study = "A"),
    "spanning 2 index points cannot be fitted"
  )
})

test_that("a multivariate estimate round trips to a recoverable meta model", {
  set.seed(15)
  meanlog <- 1.6
  sdlog <- 0.5
  # A study that published draws of the mean and standard deviation it fitted.
  parameter_draws <- cbind(
    meanlog = rnorm(500, meanlog, 0.02), sdlog = rnorm(500, sdlog, 0.01)
  )
  estimates <- suppressMessages(as_epidist_estimates_data(
    as_epidist_multivariate(parameter_draws),
    study = "A", family = "lognormal", cens_adjusted = 1
  ))
  prep <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(prep$obs_type, 7L)
  standata <- suppressMessages(epidist(prep, fn = brms::make_standata))
  slots <- .meta_row_slots(1, list(data = standata))
  recovered <- stats::optim(
    c(1, 0),
    function(par) {
      return(-.meta_row_log_lik(
        slots, "plnorm", list(meanlog = par[1], sdlog = exp(par[2]))
      ))
    }
  )
  expect_equal(recovered$par[1], meanlog, tolerance = 0.05)
  expect_equal(exp(recovered$par[2]), sdlog, tolerance = 0.1)
})

test_that("one study can contribute two multivariate objects", {
  set.seed(11)
  first <- as_epidist_multivariate(
    cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
  )
  second <- as_epidist_multivariate(
    cbind(mean = rnorm(500, 6.5, 0.3), sd = rnorm(500, 3.1, 0.2))
  )
  combined <- suppressMessages(as_epidist_estimates_data(list(
    as_epidist_estimates_data(
      first, study = "A", relative_obs_time = 20, trunc_adjusted = FALSE
    ),
    as_epidist_estimates_data(second, study = "A")
  )))
  expect_length(.estimates_vcov(combined), 2)
  expect_identical(combined$mvn_id, c("A", "A", "A_2", "A_2"))
  prep <- suppressMessages(as_epidist_meta_model(estimates = combined))
  expect_identical(prep$obs_type, c(7L, 7L))
  expect_identical(prep$trunc_adjusted, c(0L, 1L))
})
