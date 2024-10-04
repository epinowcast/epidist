# Make this data available for other tests
prep_obs <- as_latent_individual(sim_obs)
family_lognormal <- epidist_family(prep_obs, family = "lognormal")
family_gamma <- epidist_family(prep_obs, family = "gamma")
