test_that("add_natural_scale_mean_sd.lognormal_samples works with posterior samples from the latent lognormal model", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  ld_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu")
  ld_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma")
  lp_mu_melt <- reshape2::melt(
    ld_mu, varnames = c("draw", "index"), value.name = "mu"
  )
  lp_sigma_melt <- reshape2::melt(
    ld_sigma, varnames = c("draw", "index"), value.name = "sigma"
  )
  lp_melt <- dplyr::left_join(lp_mu_melt, lp_sigma_melt)
  class(linpred_melt) <- c(class(linpred_melt), "lognormal_samples")
  dt <- data.table::as.data.table(linpred_melt)
  x <- add_natural_scale_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("draw", "index", "mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_natural_scale_mean_sd.lognormal_samples works with simulated lognormal distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    mu = rnorm(n = 100, mean = 1.8, sd = 0.1),
    sigma = rnorm(n = 100, mean = 0.5, sd = 0.05)
  )
  class(dt) <- c(class(dt), "lognormal_samples")
  x <- add_natural_scale_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_natural_scale_mean_sd.gamma_samples works with simulated gamma distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    shape = rnorm(n = 100, mean = 2, sd = 0.1),
    rate = rnorm(n = 100, mean = 3, sd = 0.2)
  )
  dt[, mu := shape / rate]
  class(dt) <- c(class(dt), "gamma_samples")
  x <- add_natural_scale_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("shape", "rate", "mu", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})
