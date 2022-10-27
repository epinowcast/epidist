naive_delay <- function(formula = bf(delay_daily ~ 1, sigma ~ 1), data,
                        fn = brms::brm, family = "lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

truncation_adjusted_delay <- function(
  formula = bf(
    delay_daily | trunc(lb = 1, ub = censored_obs_time) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family ="lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

censoring_adjusted_delay <- function(
  formula = bf(
    delay_daily | trunc(lb = 1, ub = censored_obs_time) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family ="lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}


# Correct for censoring
censor_model <- brm(
  bf(daily_delay | cens(censored, day_after_delay) ~ 1, sigma ~ 1),
  data = double_truncated_obs, family = lognormal(),
  backend = "cmdstanr", adapt_delta = 0.9
)

# Less close than truncation but better than naive model
summary(censor_model)

# Correct for double interval censoring and truncation
censor_trunc_model <- brm(
  bf(
    daily_delay | trunc(lb = 1, ub = censored_obs_time) +
      cens(censored, day_after_delay) ~ 1,
    sigma ~ 1
  ),
  data = double_truncated_obs, family = lognormal(), backend = "cmdstanr"
)

# Recover underlying distribution
# As the growth rate increases and with short delays we may still see a bias
# as we have a censored observation time
summary(censor_trunc_model)

# Model censoring as a latent process (WIP)
# For this model we need to use a custom  brms family and so
# the code is significantly more complex.

# Custom family for latent censoring and truncation
fit_latent_lognormal <- function(fn = brm, ...) {
  latent_lognormal <- custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma", "pwindow", "swindow"),
    links = c("identity", "log", "identity", "identity"),
    lb = c(NA, 0, 0, 0),
    ub = c(NA, NA, 1, 1),
    type = "real",
    vars = c("vreal1[n]", "vreal2[n]")
  )

  stan_funs <- "
  real latent_lognormal_lpdf(real y, real mu, real sigma, real pwindow,
                              real swindow, real sevent,
                              real end_t) {
    real p = y + pwindow;
    real s = sevent + swindow;
    real d = s - p;
    real obs_time = end_t - p;
    return lognormal_lpdf(d | mu, sigma) - lognormal_lcdf(obs_time | mu, sigma);
    }
  "

  stanvars <- stanvar(block = "functions", scode = stan_funs)

  # Set up shared priors ----------------------------------------------------
  priors <- c(
    prior(uniform(0, 1), class = "b", dpar = "pwindow", lb = 0, ub = 1),
    prior(uniform(0, 1), class = "b", dpar = "swindow", lb = 0, ub = 1)
  )

  fit <- fn(family = latent_lognormal, stanvars = stanvars, prior = priors, ...)

  return(fit)
}
# Fit latent lognormal model
latent_model <- fit_latent_lognormal(
  bf(primary_event | vreal(secondary_event, max_t) ~ 1, sigma ~ 1,
  pwindow ~ 0 + as.factor(id), swindow ~ 0 + as.factor(id)),
  data = truncated_obs, backend = "cmdstanr", fn = brm,
  adapt_delta = 0.95
)

# Should also see parameter recovery using this method though 
# run-times are much higher and the model is somewhat unstable.
summary(latent_model)
