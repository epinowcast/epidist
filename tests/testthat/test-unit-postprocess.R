test_that("add_natural_scale_mean_sd.lognormal_samples works with posterior samples from the latent lognormal model", { # nolint: line_length_linter.
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  
  linpred_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu")
  linpred_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma")
  
  linpred_mu_melt <- reshape2::melt(linpred_mu, varnames = c("draw", "index"), value.name = "mu")
  linpred_sigma_melt <- reshape2::melt(linpred_sigma, varnames = c("draw", "index"), value.name = "sigma")
  
  linpred_melt <- dplyr::left_join(linpred_mu_melt, linpred_sigma_melt)
  class(linpred_melt) <- c(class(linpred_melt), "lognormal_samples")
  dt <- data.table::as.data.table(linpred_melt)
  x <- add_natural_scale_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("draw", "index", "mu", "sigma", "mean", "sd"))
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
