test_that("add_natural_scale_mean_sd.lognormal_samples", { # nolint: line_length_linter.
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(data = prep_obs, seed = 1)
  
  linpred_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu")
  linpred_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma")
  
  linpred_mu_melt <- reshape2::melt(linpred_mu, varnames = c("draw", "index"), value.name = "mu")
  linpred_sigma_melt <- reshape2::melt(linpred_sigma, varnames = c("draw", "index"), value.name = "sigma")
  
  linpred_melt <- dplyr::left_join(linpred_mu_melt, linpred_sigma_melt)
  class(linpred_melt) <- c(class(linpred_melt), "lognormal_samples")
  x <- add_natural_scale_mean_sd(data.table::as.data.table(linpred_melt))
  # Test x here
})

test_that("add_natural_scale_mean_sd.lognormal_samples", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    shape = rnorm(n = 100, mean = 2, sd = 0.1),
    rate = rnorm(n = 100, mean = 3, sd = 0.2)
  )
  dt[, mu := shape / rate]
  class(dt) <- c(class(dt), "gamma_samples")
  x <- add_natural_scale_mean_sd(dt)
  # Test x here
})

# Below contains implementation notes:

# Need to modify this (i.e. "latent_lognormal")
fit$family

# (Which includes these names for the link functions needed)
fit$family$link
fit$family$link_sigma

# To work with these existing brms prediction functions
brms::posterior_epred(fit)
brms::posterior_predict(fit)

pp <- brms::prepare_predictions(fit)
pp$dpars$mu$fe$b
pp$dpars$sigma$fe$b

fit$family$family <- "lognormal"
fit$family$name <- "lognormal"

str(fit$family)
brms::posterior_predict(fit)
