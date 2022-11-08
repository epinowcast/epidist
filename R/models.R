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
    delay_central | vreal(obs_at, pwindow_upr, swindow_upr) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm,
  family = brms::custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  ),
  scode_functions = "
    real latent_lognormal_lpdf(vector y, vector mu, vector sigma,
                               vector pwindow, vector swindow,
                               array[] real obs_t) {
      int n = num_elements(y);
      vector[n] d = y - pwindow + swindow;
      vector[n] obs_time = to_vector(obs_t) - pwindow;
      return lognormal_lpdf(d | mu, sigma) -
        lognormal_lcdf(obs_time | mu, sigma);
      }
  ",
  scode_parameters = "
    vector<lower = 0, upper = to_vector(vreal2)>[N] pwindow;
    vector<lower = 0, upper = to_vector(vreal3)>[N] swindow;
  ",
  scode_prior = "
    pwindow ~ uniform(0, to_vector(vreal2));
    swindow ~ uniform(0, to_vector(vreal3));
  ",
  ...
) {
  
  stanvars_functions <- brms::stanvar(
    block = "functions", scode = scode_functions
  )
  stanvars_parameters <- brms::stanvar(
    block = "parameters", scode = scode_parameters
  )
  stanvars_prior <- brms::stanvar(block = "model", scode = scode_prior)
  
  stanvars_all <- stanvars_functions + stanvars_parameters + stanvars_prior
  
  data <- data |>
    data.table::copy() |>
    DT(, id := 1:.N) |>
    DT(, obs_t := obs_at - ptime_lwr) |>
    DT(, pwindow_upr := ptime_upr - ptime_lwr) |>
    DT(, swindow_upr := stime_upr - stime_lwr) |>
    DT(, delay_central := stime_lwr - ptime_lwr)
  
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
