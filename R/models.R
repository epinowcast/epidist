#' Estimate delays naively
#' @export
naive_delay <- function(formula = brms::bf(delay_daily ~ 1, sigma ~ 1), data,
                        fn = brms::brm, family = "lognormal", ...) {

  data <- drop_zero(data)
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

  data <- drop_zero(data)

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

#' Estimate delays adjusted forcensoring using a
#' latent model
#' @export
latent_censoring_adjusted_delay <- function(
  formula = brms::bf(
    delay_central | vreal(pwindow_upr, swindow_upr) ~ 1,
    sigma ~ 1
  ), data, fn = brms::brm,
  family = brms::custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("pwindow", "swindow"),
    loop = FALSE
  ),
  scode_functions = "
    real latent_lognormal_lpdf(vector y, vector mu, vector sigma,
                               vector pwindow, vector swindow) {
      int n = num_elements(y);
      vector[n] d = y - pwindow + swindow;
      return lognormal_lpdf(d | mu, sigma);
      }
  ",
  scode_parameters = "
    vector<lower = 0, upper = to_vector(vreal1)>[N] pwindow;
    vector<lower = 0, upper = to_vector(vreal2)>[N] swindow;
  ",
  scode_prior = "
    pwindow ~ uniform(0, to_vector(vreal1));
    swindow ~ uniform(0, to_vector(vreal2));
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
    delay_daily | trunc(lb = 1, ub = censored_obs_time) ~ 1, sigma ~ 1
  ), data, fn = brms::brm, family = "lognormal", ...) {

  data <- drop_zero(data)

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
    delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1,
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
    vector<lower = 0, upper = 1>[N] swindow_raw;
    vector<lower = 0, upper = 1>[N] pwindow_raw;
  ",
  scode_tparameters = "
    vector<lower = 0>[N] pwindow;
    vector<lower = 0>[N] swindow;
    swindow = to_vector(vreal3) .* swindow_raw;
    pwindow[noverlap] = to_vector(vreal2[noverlap]) .* pwindow_raw[noverlap];
    if (wN) {
      pwindow[woverlap] = swindow[woverlap] .* pwindow_raw[woverlap];
    }
  ",
  scode_priors = "
    swindow_raw ~ uniform(0, 1);
    pwindow_raw ~ uniform(0, 1);
  ",
  ...
) {

  data <- data |>
    data.table::copy() |>
    DT(, id := 1:.N) |>
    DT(, obs_t := obs_at - ptime_lwr) |>
    DT(, pwindow_upr := ifelse(
          stime_lwr < ptime_upr, ## if overlap
          stime_upr - ptime_lwr,
          ptime_upr - ptime_lwr
        )
    ) |>
    DT(, 
      woverlap := as.numeric(stime_lwr < ptime_upr)
    ) |>
    DT(, swindow_upr := stime_upr - stime_lwr) |>
    DT(, delay_central := stime_lwr - ptime_lwr) |>
    DT(, row_id := 1:.N)

  if (nrow(data) > 1) {
    data <- data[, id := as.factor(id)]
  }

  stanvars_functions <- brms::stanvar(
    block = "functions", scode = scode_functions
  )
  stanvars_data <- brms::stanvar(
    block = "data", scode = "int wN;",
    x = nrow(data[woverlap > 0]),
    name = "wN"
  ) +
  brms::stanvar(
    block = "data", scode = "array[N - wN] int noverlap;",
    x = data[woverlap == 0][, row_id],
    name = "noverlap"
  ) +
  brms::stanvar(
    block = "data", scode = "array[wN] int woverlap;",
    x = data[woverlap > 0][, row_id],
    name = "woverlap"
  )

  stanvars_parameters <- brms::stanvar(
    block = "parameters", scode = scode_parameters
  )
  stanvars_tparameters <- brms::stanvar(
    block = "tparameters", scode = scode_tparameters
  )
  stanvars_priors <- brms::stanvar(block = "model", scode = scode_priors)

  stanvars_all <- stanvars_functions + stanvars_data + stanvars_parameters + 
    stanvars_tparameters + stanvars_priors

  fit <- fn(
    formula = formula, family = family, stanvars = stanvars_all,
    backend = "cmdstanr", data = data,
    ...
  )
  return(fit)
}

#' Estimate delays from the backward delay distribution
#' @export
backward_delay <- function(data, data_cases, ...) {
  data <- drop_zero(data)

  if (!all(c("time", "cases") %in% colnames(data_cases))) {
    stop(
      "`data_cases` must be a data.frame object containing `time` and `cases` columns" # nolint
    )
  }

  tmin <- pmin(min(data$ptime_daily), min(data_cases$time))
  tmax <- pmax(max(data$stime_daily), max(data_cases$time))

  data_cases_tmp <- data.table(
    time = tmin:tmax,
    cases = 1e-3
  )

  data_cases_tmp[match(data_cases$time, time), cases := data_cases$cases]
  data_cases <- data_cases_tmp

  cases <- data_cases$cases
  tmin <- min(data_cases$time)
  tlength <- nrow(data_cases)

  model <- cmdstan_model("data/models/lognormal_dynamical_full.stan")

  standata <- list(
    N = nrow(data),
    delay_lwr = data$delay_lwr,
    delay_upr = data$delay_upr,
    stime_daily = data$stime_daily,
    tlength = nrow(data_cases),
    tmin = min(data_cases$time),
    cases = data_cases$cases
  )

  fit <- model$sample(data = standata, ...)

}

## this doesn't run right now...
#' Estimate delays from the backward delay distribution + brms
#' @param data_cases data frame consisting of integer time column and incidence column
backward_delay_brms <- function(
    formula = brms::bf(
      delay_lwr | cens(censored, delay_upr) ~ 1, sigma ~ 1
    ),
    data,
    data_cases,
    fn = brms::brm,
    family = "lognormal",
    scode_data = "
      array[N] int stime_daily;
      int<lower=1> tlength; // time series length
      int tmin;
  
      array[tlength] int cases;
    ",
    scode_tparameters = "
      array[tlength] real cdenom;
  
      cdenom[1] = 0;
  
      for (i in 2:tlength) {
        cdenom[i] = 0;
        for (j in 1:(i-1)) {
          if (j==1) {
            cdenom[i] += 
              exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)) +
                log(cases[i-j]));
          } else {
            cdenom[i] += 
              exp(
                log_diff_exp(
                  lognormal_lcdf(j | Intercept, exp(Intercept_sigma)), 
                  lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma))
                ) + 
                log(cases[i-j])
              );
          }
        }
      }
    ",
    scode_model = "
      if (!prior_only) {
        for (i in 1:N) {
          target += - log(cdenom[stime_daily[i] - tmin + 1]);
        }
      }
    ",
    ...) {

  if (!all(c("time", "cases") %in% colnames(data_cases))) {
    stop(
      "`data_cases` must be a data.frame containing `time` and `cases` columns"
    )
  }
  cols <- colnames(data)[map_lgl(data, is.integer)]
  data <- data |>
    DT(, (cols) := lapply(.SD, as.double), .SDcols = cols)

  data <- pad_zero(data)

  data_cases_tmp <- data.table(
    time = min(data_cases$time):max(data_cases$time),
    cases = 1e-3
  )

  data_cases_tmp[match(data_cases$time, time), cases := data_cases$cases]
  data_cases <- data_cases_tmp

  cases <- data_cases$cases
  tmin <- min(data_cases$time)
  tlength <- nrow(data_cases)

  stanvars_data <- c(
    brms::stanvar(
      x = tmin, block = "data",
      scode = "int tmin;",
      name = "tmin"
    ),
    brms::stanvar(
      x = tlength, block = "data",
      scode = "int<lower=1> tlength; // time series length",
      name = "tlength"
    ),
    brms::stanvar(
      x = data$stime_daily, block = "data",
      scode = "array[N] int stime_daily;",
      name = "stime_daily"
    ),
    brms::stanvar(
      x = cases, block = "data",
      scode = "array[tlength] int cases;",
      name = "cases"
    )
  )

  stanvars_tparameters <- brms::stanvar(
    block = "tparameters", scode = scode_tparameters
  )

  stanvars_model <- brms::stanvar(
    block = "model", scode = scode_model
  )

  stanvars_gp <- brms::stanvar(
    block = "genquant", scode = "
        array[tlength] real backwardmean;
  
        for (i in 1:tlength) {
          backwardmean[i] = 0;
          
          // this is quite approximate...
          // but sort of the best we can do without sacrificing a ton of computational power
          for (j in 1:(i-1)) {
            backwardmean[i] += exp(log_diff_exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)), 
                lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma))) + log(cases[i-j]) + log(j-0.5) - log(cdenom[i]));
          }
        }
  "
  )

  stanvars_all <- stanvars_data + stanvars_tparameters + stanvars_model +
   stanvars_gp

  ## TODO: need to figure out
  fit <- fn(
    formula, data = data,
    family = family,
    stanvars = stanvars_all,
    backend = "cmdstanr",
    ...
  )

  return(fit)
}
