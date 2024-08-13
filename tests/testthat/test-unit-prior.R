data <- as_latent_individual(sim_obs)
formula <- brms::bf(mu ~ 1, sigma ~ 1)
family <- brms::lognormal()
prior <- NULL
fn <- brms::brm

epidist_validate(data)
epidist_family <- epidist_family(data, family)
epidist_formula <- epidist_formula(
  data = data, family = epidist_family, formula = formula
)

# Start of epidist_prior
epidist_validate(data)
default <- brms::default_prior(data, family, formula)
model <- epidist_model_prior(data, formula)
family <  epidist_family_prior(family, formula)
out <- replace_brms_prior(default, model)
out <- replace_brms_prior(out, family)
out <- replace_brms_prior(out, prior)