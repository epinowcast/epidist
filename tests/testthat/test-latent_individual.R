# Assumes that tests/testthat/setup.R has been run

prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

formula <- epidist_formula(prep_obs)
# TODO: add tests for formula
formula

family <- epidist_family(prep_obs)
# TODO: add tests for formula
family

priors <- epidist_priors(prep_obs)
# TODO: add tests for formula
priors

stancode <- epidist_stancode(prep_obs)
# TODO: add tests for stancode
stancode

fit <- epidist(
  data = prep_obs,
  formula = formula,
  family = family,
  priors = priors,
  stancode = stancode
)
# TODO: add tests for epidist
# Note this is the same thing as fit2 <- epidist(prep_obs)

# Could also do tests on the stancode
stancode <- epidist(prep_obs, fn = brms::make_stancode)
stancode

# Could also test other distribution functionality
fit_gamma <- epidist(
  data = prep_obs,
  formula = formula,
  family = epidist_family(prep_obs, family = "gamma"),
  priors = priors,
  stancode = epidist_stancode(
    prep_obs,
    family = epidist_family(prep_obs, family = "gamma")
  )
)

# Could also test how default methods work
x <- list()
epidist_prepare(x)
epidist_family(x)
epidist_formula(x)
epidist_priors(x)
epidist_stancode(x)