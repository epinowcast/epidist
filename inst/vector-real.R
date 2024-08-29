source("~/Documents/cfa/delays/epidist/tests/testthat/setup.R", echo = TRUE)
prep_obs <- as_latent_individual(sim_obs)
set.seed(1)

# Fails
fit <- epidist(
  data = prep_obs,
  formula = brms::bf(mu ~ 1),
  seed = 1,
  silent = 2
)

data <- prep_obs
formula <- brms::bf(mu ~ 1, sigma ~ 1)
prior <- NULL

epidist_validate(data)
epidist_family <- epidist_family(data, family)
epidist_formula <- epidist_formula(
  data = data, family = epidist_family, formula = formula
)

# Get the dpar names
# Only include prior bits if the dpars are there
epidist_prior <- epidist_prior(
  data = data, family = family, formula = epidist_formula
)
