epidist_data.ltcad <- function() {
  data <- data.table::as.data.table(data)
  data[, id := 1:.N]
  data[, obs_t := obs_at - ptime_lwr]
  data[, pwindow_upr := ifelse(
    stime_lwr < ptime_upr, ## if overlap
    stime_upr - ptime_lwr,
    ptime_upr - ptime_lwr
  )]
  data[, woverlap := as.numeric(stime_lwr < ptime_upr)]
  data[, swindow_upr := stime_upr - stime_lwr]
  data[, delay_central := stime_lwr - ptime_lwr]
  data[, row_id := 1:.N]
  
  if (nrow(data) > 1) {
    data <- data[, id := as.factor(id)]
  }
}

epidist_stancode.ltcad <- function() {
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

  return(stanvars_all)
}

epidist_priors.ltcad <- function() {
  # ...  
}

epidist_formula.ltcad <- function() {
  # ...  
}

epidist.ltcad <- function() {
  fit <- fn(
    formula = formula, family = family, stanvars = stanvars_all,
    backend = "cmdstanr", data = data,
    ...
  )
  return(fit)
}

#' #' Estimate delays adjusted for right truncation and censoring using a
#' #' latent model
#' #' 
#' #' @family model
#' #' @export
#' latent_truncation_censoring_adjusted_delay <- function(
#'     formula = brms::bf(
#'       delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1,
#'       sigma ~ 1
#'     ), data, fn = brms::brm,
#'     family = brms::custom_family(
#'       "latent_lognormal",
#'       dpars = c("mu", "sigma"),
#'       links = c("identity", "log"),
#'       lb = c(NA, 0),
#'       ub = c(NA, NA),
#'       type = "real",
#'       vars = c("pwindow", "swindow", "vreal1"),
#'       loop = FALSE
#'     ),
#'     scode_functions = "
#'     real latent_lognormal_lpdf(vector y, vector mu, vector sigma,
#'                                vector pwindow, vector swindow,
#'                                array[] real obs_t) {
#'       int n = num_elements(y);
#'       vector[n] d = y - pwindow + swindow;
#'       vector[n] obs_time = to_vector(obs_t) - pwindow;
#'       return lognormal_lpdf(d | mu, sigma) - 
#'         lognormal_lcdf(obs_time | mu, sigma);
#'       }
#'   ",
#'     scode_parameters = "
#'     vector<lower = 0, upper = 1>[N] swindow_raw;
#'     vector<lower = 0, upper = 1>[N] pwindow_raw;
#'   ",
#'     scode_tparameters = "
#'     vector<lower = 0>[N] pwindow;
#'     vector<lower = 0>[N] swindow;
#'     swindow = to_vector(vreal3) .* swindow_raw;
#'     pwindow[noverlap] = to_vector(vreal2[noverlap]) .* pwindow_raw[noverlap];
#'     if (wN) {
#'       pwindow[woverlap] = swindow[woverlap] .* pwindow_raw[woverlap];
#'     }
#'   ",
#'     scode_priors = "
#'     swindow_raw ~ uniform(0, 1);
#'     pwindow_raw ~ uniform(0, 1);
#'   ",
#'     ...
#' ) {
#' }