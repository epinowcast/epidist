prep_obs <- as_latent_individual(sim_obs)
family <- epidist_family(prep_obs, family = "lognormal")
formula <- epidist_formula(data = prep_obs, family = fam, form = list(mu ~ 1, sigma ~ 1))
formula
