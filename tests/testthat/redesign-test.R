prep_obs <- as_latent_individual(sim_obs)
epidist::epidist(
  prep_obs,
  formula = list(mu ~ 1, sigma ~ 1),
  family = brms::lognormal(),
  fn = brms::brm
)
