#' @method epidist_prepare epidist_ltcad
#' @export
epidist_prepare.epidist_ltcad <- function(data) {
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
  
  return(data)
}

#' @method epidist_stancode epidist_ltcad
#' @export
epidist_stancode.epidist_ltcad <- function(data) {
  stanvars_functions <- brms::stanvar(
    block = "functions", scode = epidist_stan_chunk("functions.stan")
  )
  
  stanvars_data <- brms::stanvar(
    block = "data",
    scode = "int wN;",
    x = nrow(data[woverlap > 0]),
    name = "wN"
  ) +
    brms::stanvar(
      block = "data",
      scode = "array[N - wN] int noverlap;",
      x = data[woverlap == 0][, row_id],
      name = "noverlap"
    ) +
    brms::stanvar(
      block = "data",
      scode = "array[wN] int woverlap;",
      x = data[woverlap > 0][, row_id],
      name = "woverlap"
    )
  
  stanvars_parameters <- brms::stanvar(
    block = "parameters", scode = epidist_stan_chunk("parameters.stan")
  )
  
  stanvars_tparameters <- brms::stanvar(
    block = "tparameters", scode = epidist_stan_chunk("tparameters.stan")
  )
  
  stanvars_priors <- brms::stanvar(
    block = "model", scode = epidist_stan_chunk("priors.stan")
  )
  
  stanvars_all <- stanvars_functions + stanvars_data + stanvars_parameters +
    stanvars_tparameters + stanvars_priors

  return(stanvars_all)
}

#' @method epidist_priors epidist_ltcad
#' @export
epidist_priors.epidist_ltcad <- function(data) {
  return(NULL)
}

#' Define a formula for the ltcad model
#' 
#' @param delay_central
#' @param sigma
#' @method epidist_formula epidist_ltcad
#' @export
epidist_formula.epidist_ltcad <- function(data, delay_central = ~ 1,
                                          sigma = ~ 1) {
  delay_equation <- paste0(
    "delay_central | vreal(obs_t, pwindow_upr, swindow_upr)",
    paste(delay_central, collapse = " ")
  )

  sigma_equation <- paste0("sigma", paste(sigma, collapse = " "))
  form <- brms::bf(as.formula(delay_equation), as.formula(sigma_equation))
  return(form)
}

#' @method epidist_family epidist_ltcad
#' @export
epidist_family.epidist_ltcad <- function(data) {
  brms::custom_family(
    "latent_lognormal",
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  )
}

#' @method epidist epidist_ltcad
#' @export
epidist.epidist_ltcad <- function(data, formula = epidist_formula(data),
                                  family = epidist_family(data),
                                  priors = epidist_priors(data),
                                  custom_stancode = epidist_stancode(data),
                                  dry = FALSE,
                                  ...) {
  
  fn <- ifelse(dry, brms::make_stancode, brms::brm)
  
  fit <- fn(
    formula = formula, family = family, stanvars = custom_stancode,
    backend = "cmdstanr", data = data, ...
  )
  
  class(fit) <- c(class(fit), "epidist_fit")
  
  return(fit)
}
