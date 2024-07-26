test_that("add_mean_sd.lognormal_samples works with posterior samples from the latent lognormal model", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  lp_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu") |>
    as.table() |>
    as.data.table()
  names(lp_mu) <- c("draw", "index", "mu")
  lp_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma") |>
    as.table() |>
    as.data.table(value.name = "sigma")
  names(lp_sigma) <- c("draw", "index", "sigma")
  lp <- dplyr::left_join(lp_mu, lp_sigma)
  class(lp) <- c(class(lp), "lognormal_samples")
  x <- add_mean_sd(lp)
  expect_s3_class(x, "data.table")
  expect_named(x, c("draw", "index", "mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
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
