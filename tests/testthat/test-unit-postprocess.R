prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(data = prep_obs, seed = 1)
fit$fit

# Names for the link functions needed
fit$family$link
fit$family$link_sigma
