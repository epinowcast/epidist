epidist_data.epidist_ltcad <- function() {
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

epidist_stancode.epidist_ltcad <- function() {
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

epidist_priors.epidist_ltcad <- function() {
  # ...  
}

epidist_formula.epidist_ltcad <- function() {
  brms::bf(
    delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1,
    sigma ~ 1
  )
}

epidist_family.epidist_ltcad <- function() {
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

epidist.epidist_ltcad <- function() {
  fn <- brms::brm
  fit <- fn(
    formula = formula, family = family, stanvars = stanvars_all,
    backend = "cmdstanr", data = data, ...
  )
  
  return(fit)
}
