test_that("predict_delay_parameters works with NULL newdata and the latent lognormal model", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(
    data = prep_obs,
    seed = 1,
    silent = 2,
    output_dir = fs::dir_create(tempfile())
  )
  pred <- predict_delay_parameters(fit)
  expect_s3_class(pred, "data.table")
  expect_named(pred, c("index", "draw", "mu", "sigma", "mean", "sd"))
  expect_true(all(pred$mean > 0))
  expect_true(all(pred$sd > 0))
  expect_equal(length(unique(pred$index)), nrow(prep_obs))
  expect_equal(length(unique(pred$draw)), summary(fit)$total_ndraws)
})

test_that("predict_delay_parameters accepts newdata arguments", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(
    data = prep_obs,
    seed = 1,
    silent = 2,
    output_dir = fs::dir_create(tempfile())
  )
  n <- 5
  pred <- predict_delay_parameters(fit, newdata = prep_obs[1:n, ])
  expect_s3_class(pred, "data.table")
  expect_named(pred, c("index", "draw", "mu", "sigma", "mean", "sd"))
  expect_true(all(pred$mean > 0))
  expect_true(all(pred$sd > 0))
  expect_equal(length(unique(pred$index)), 5)
  expect_equal(length(unique(pred$draw)), summary(fit)$total_ndraws)
})

test_that("add_mean_sd.lognormal_samples works with simulated lognormal distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    mu = rnorm(n = 100, mean = 1.8, sd = 0.1),
    sigma = rnorm(n = 100, mean = 0.5, sd = 0.05)
  )
  class(dt) <- c(class(dt), "lognormal_samples")
  x <- add_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_mean_sd.gamma_samples works with simulated gamma distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    shape = rnorm(n = 100, mean = 2, sd = 0.1),
    rate = rnorm(n = 100, mean = 3, sd = 0.2)
  )
  dt[, mu := shape / rate]
  class(dt) <- c(class(dt), "gamma_samples")
  x <- add_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("shape", "rate", "mu", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})
