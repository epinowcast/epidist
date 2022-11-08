#' Estimate delays naively
#' @export
naive_delay <- function(formula = brms::bf(delay_daily ~ 1, sigma ~ 1), data,
                        fn = brms::brm, family = "lognormal", ...) {

  data <- pad_zero(data)
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays with filtering of the most recent data
#' @export
filtered_naive_delay <- function(
  formula = brms::bf(delay_daily ~ 1, sigma ~ 1), data, fn = brms::brm,
  family = "lognormal", truncation = 10, ...) {
  data <- data |>
    data.table::copy() |>
    ## NEED TO FILTER BASED ON PTIME
    DT(ptime_daily <= (obs_at - truncation))

  data <- pad_zero(data)

  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays adjusted for censoring
#' @export
censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_lwr | cens(censored, delay_upr) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {
  data <- pad_zero(data)
  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays with filtering of the most recent data and
#' censoring adjustment
#' @export
filtered_censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_lwr | cens(censored, delay_upr) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", truncation = 10, ...) {

  data <- data |>
    data.table::copy() |>
    DT(ptime_daily <= (obs_at - truncation))

  data <- pad_zero(data)

  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays adjusted for right truncation
#' @export
truncation_adjusted_delay <- function(
  formula = brms::bf(
    delay_daily | trunc(lb = 1e-3, ub = censored_obs_time) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {

  data <- pad_zero(data)

  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays adjusted for censoring and right truncation
#' @export
truncation_censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_lwr | cens(censored, delay_upr) +
      trunc(lb = 1e-3, ub = censored_obs_time) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {

  data <- pad_zero(data)

  fn(
    formula, data = data, family = family, backend = "cmdstanr", ...
  )
}

#' Estimate delays adjusted for right truncation and censoring using a
#' latent model
#' @export
latent_truncation_censoring_adjusted_delay <- function(
  formula = brms::bf(
    ptime_daily | vreal(stime_daily, obs_at) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm,
  family = brms::custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("delay", "obs_time"),
    loop = FALSE
  ),
  scode_functions = "
  real latent_lognormal_lpdf(vector y, vector mu, vector sigma, vector delay, vector obs_time) {
    return lognormal_lpdf(delay | mu, sigma) - lognormal_lcdf(obs_time | mu, sigma);
    }
  ",
  scode_parameters = "
    vector<lower=0, upper=1>[N] pwindow;
    vector<lower=0, upper=1>[N] swindow;
  ",
  scode_tparameters = "
    vector[N] ptime;
    vector[N] stime;
    vector[N] delay;
    vector[N] obs_time;
    
    ptime = Y + pwindow;
    stime = to_vector(vreal1) + swindow;
    delay = stime - ptime;
    obs_time = to_vector(vreal2) - ptime;
  ",
  scode_prior = "
    pwindow ~ uniform(0, 1);
    swindow ~ uniform(0, 1);
  ",
  ...
) {
  
  stanvars_functions <- brms::stanvar(block = "functions", scode = scode_functions)
  stanvars_parameters <- brms::stanvar(block = "parameters", scode = scode_parameters)
  stanvars_tparameters <- brms::stanvar(block = "tparameters", scode = scode_tparameters)
  stanvars_prior <- brms::stanvar(block = "model", scode = scode_prior)
  
  stanvars_all <- stanvars_functions + stanvars_parameters + stanvars_tparameters + stanvars_prior
  
  data <- data |>
    data.table::copy() |>
    DT(, id := 1:.N) |>
    DT(, obs_t := obs_at - ptime_daily)
  
  if (nrow(data) > 1) {
    data <- data[, id := as.factor(id)]
  }
  
  fit <- fn(
    formula = formula, family = family, stanvars = stanvars_all,
    backend = "cmdstanr", data = data, ...
  )
  return(fit)
}

#' Estimate delays adjusted for epidemic growth rate
#' @export
exponential_delay <- function(
  formula = brms::bf(
    delay_daily | vreal(r, max_delay) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm,
  family = brms::custom_family(
    "exponential_lognormal",
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("vreal1[n]", "vreal2[n]", "x_r", "x_i")
  ),
  scode_functions = "
  real denom(real x, real xc, real[] theta,
                      real[] x_r, int[] x_i) {
    real r = theta[1];
    real mu = theta[2];
    real sigma = theta[3];
    
    return exp(lognormal_lpdf(x | mu, sigma) - r * x);                    
  }
  
  real exponential_lognormal_lpdf(real y, real mu, real sigma, real r,
                             real max_delay, data real[] x_r, data int[] x_i) {
    // it looks like we can skip - r * y because that's just a contant value???
    return lognormal_lpdf(y | mu, sigma) - log(integrate_1d(denom, 0, max_delay, {r, mu, sigma}, x_r, x_i, 1e-4));
  }
  ",
  scode_tdata = "
  real x_r[0]; 
  int x_i[0];
  ",
  ...
) {
  
  stanvars_functions <- brms::stanvar(block = "functions", scode = scode_functions)
  stanvars_tdata <- brms::stanvar(block="tdata", scode=scode_tdata)
    
  stanvars_all <- stanvars_functions + stanvars_tdata
  
  data <- pad_zero(data)
  
  fit <- fn(
    formula = formula, family = family, stanvars = stanvars_all,
    backend = "cmdstanr", data = data, ...
  )
  
  
  return(fit)
}
