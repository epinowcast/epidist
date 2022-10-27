naive_delay <- function(formula = brms::bf(delay_daily ~ 1, sigma ~ 1), data,
                        fn = brms::brm, family = "lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_lwr | cens(censored, delay_upr) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

truncation_adjusted_delay <- function(
  formula = brms::bf(
    # The low bound here was present to deal with issues having zeros in the
    # data. This is definitely not correct so needs some thought.
    delay_daily | trunc(lb = 1, ub = censored_obs_time) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family ="lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

truncation_censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_lwr | cens(censored, delay_upr) +
      trunc(lb = 1, ub = censored_obs_time) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

latent_truncation_censoring_adjusted_delay <- function(
  formula = brms::bf(
    ptime | vreal(stime, obs_at) ~ 1,
    sigma ~ 1,
    pwindow ~ 0 + as.factor(id),
    swindow ~ 0 + as.factor(id)
  ), data, fn = brms::brm,
  family = custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma", "pwindow", "swindow"),
    links = c("identity", "log", "identity", "identity"),
    lb = c(NA, 0, 0, 0),
    ub = c(NA, NA, 1, 1),
    type = "real",
    vars = c("vreal1[n]", "vreal2[n]")
  ),
  scode = "
  real latent_lognormal_lpdf(real y, real mu, real sigma, real pwindow,
                              real swindow, real stime,
                              real obs_t) {
    real p = y + pwindow;
    real s = stime + swindow;
    real d = s - p;
    real obs_time = obs_t - p;
    return lognormal_lpdf(d | mu, sigma) - lognormal_lcdf(obs_time | mu, sigma);
    }
  ",
  priors = c(
    prior(uniform(0, 1), class = "b", dpar = "pwindow", lb = 0, ub = 1),
    prior(uniform(0, 1), class = "b", dpar = "swindow", lb = 0, ub = 1)
  ),
  ...
) {

  stanvars <- brms::stanvar(block = "functions", scode = scode)
  
  data <- data |>
    data.table::copy() |>
    DT(, id := 1:.N)
  
  fit <- fn(
    formula = formula, family = family, stanvars = stanvars, prior = priors,
    backend = "cmdstanr", data = data, ...
  )
  return(fit)
}
