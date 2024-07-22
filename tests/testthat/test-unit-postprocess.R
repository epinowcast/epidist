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

fit$family$family <- "lognormal"
fit$family$name <- "lognormal"

str(fit$family)
brms::posterior_predict(fit)
