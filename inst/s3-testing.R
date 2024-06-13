meanlog <- 1.8
sdlog <- 0.5
obs_time <- 25
sample_size <- 200

obs_cens_trunc <- simulate_gillespie() |>
  simulate_secondary(
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time)

obs_cens_trunc_samp <-
  obs_cens_trunc[sample(seq_len(.N), sample_size, replace = FALSE)]

obs <- obs_cens_trunc_samp

prep_obs <- epidist_prepare(obs, model = "ltcad")

formula <- epidist_formula(prep_obs)
formula

family <- epidist_family(prep_obs)
family

priors <- epidist_priors(prep_obs)
priors

stancode <- epidist_stancode(prep_obs)
stancode

fit <- epidist(
  data = prep_obs,
  formula = formula,
  family = family,
  priors = priors,
  stancode = stancode
)

# This is the same thing as fit2 <- epidist(prep_obs)

stancode <- epidist(prep_obs, fn = brms::make_stancode)
stancode

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

# Test the defaults

x <- list()
epidist_prepare(x)
epidist_family(x)
epidist_formula(x)
epidist_priors(x)
epidist_stancode(x)
