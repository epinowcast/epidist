source("~/Documents/cfa/delays/epidist/tests/testthat/setup.R", echo = TRUE)
prep_obs <- as_latent_individual(sim_obs)
set.seed(1)

# Fails
fit <- epidist(
  data = prep_obs,
  formula = brms::bf(mu ~ 1),
  seed = 1
)
