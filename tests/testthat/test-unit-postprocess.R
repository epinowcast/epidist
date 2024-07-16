prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(data = prep_obs, seed = 1)
fit$fit
