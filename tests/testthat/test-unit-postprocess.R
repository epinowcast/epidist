set.seed(1)
prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(data = prep_obs, seed = 1)

# Need to modify this (i.e. "latent_lognormal")
fit$family

# (Which includes these names for the link functions needed)
fit$family$link
fit$family$link_sigma

# To work with these existing brms prediction functions
brms::posterior_epred(fit)
brms::posterior_predict(fit)

linpred_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu")
linpred_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma")

linpred_mu_melt <- reshape2::melt(linpred_mu, varnames = c("aaa", "bbb"), value.name = "mu")
linpred_sigma_melt <- reshape2::melt(linpred_sigma, varnames = c("aaa", "bbb"), value.name = "sigma")

linpred_melt <- dplyr::left_join(linpred_mu_melt, linpred_sigma_melt)

pp <- brms::prepare_predictions(fit)
pp$dpars$mu$fe$b
pp$dpars$sigma$fe$b

fit$family$family <- "lognormal"
fit$family$name <- "lognormal"

str(fit$family)
brms::posterior_predict(fit)
