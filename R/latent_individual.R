#' @method epidist_prepare epidist_latent_individual
#' @family latent_individual
#' @autoglobal
#' @export
epidist_prepare.epidist_latent_individual <- function(data, ...) {
  data <- data.table::as.data.table(data)
  data[, id := seq_len(.N)]
  data[, obs_t := obs_at - ptime_lwr]
  data[, pwindow_upr := ifelse(
    stime_lwr < ptime_upr, ## if overlap
    stime_upr - ptime_lwr,
    ptime_upr - ptime_lwr
  )]
  data[, woverlap := as.numeric(stime_lwr < ptime_upr)]
  data[, swindow_upr := stime_upr - stime_lwr]
  data[, delay_central := stime_lwr - ptime_lwr]
  data[, row_id := seq_len(.N)]

  if (nrow(data) > 1) {
    data <- data[, id := as.factor(id)]
  }

  return(data)
}

#' Define a formula for the latent_individual model
#'
#' @param data ...
#' @param delay_central Formula for the delay mean. Defaults to intercept only.
#' @param sigma Formula for the delay standard deviation. Defaults to intercept
#' only.
#' @param ... ...
#' @method epidist_formula epidist_latent_individual
#' @family latent_individual
#' @importFrom stats as.formula
#' @export
epidist_formula.epidist_latent_individual <- function(data, delay_central = ~ 1,
                                                      sigma = ~ 1, ...) {
  if (!inherits(delay_central, "formula")) {
    cli::cli_abort("A valid formula for delay_central must be provided")
  }
  if (!inherits(sigma, "formula")) {
    cli::cli_abort("A valid formula for sigma must be provided")
  }
  formula_vars <- c(all.vars(delay_central), all.vars(sigma))
  missing_vars <- setdiff(formula_vars, names(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      paste0(
        "The following variables are missing from data: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
  delay_equation <- paste0(
    "delay_central | vreal(obs_t, pwindow_upr, swindow_upr)",
    paste(delay_central, collapse = " ")
  )
  sigma_equation <- paste0("sigma", paste(sigma, collapse = " "))
  form <- brms::bf(as.formula(delay_equation), as.formula(sigma_equation))
  return(form)
}

#' @method epidist_family epidist_latent_individual
#' @family latent_individual
#' @export
epidist_family.epidist_latent_individual <- function(data, family = "lognormal",
                                                     ...) {
  brms::custom_family(
    paste0("latent_", family),
    dpars = c("mu", "sigma"),
    links = c("identity", "log"),
    lb = c(NA, 0),
    ub = c(NA, NA),
    type = "real",
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  )
}

#' Define priors for the model
#'
#' We provide suggested priors for the intercepts of the `meanlog` and `sdlog`
#' linear predictors. These priors are weakly informative in that they prevent
#' very large delays on the real scale. In particular:
#'
#' * `eta_meanlog ~ normal(2, 0.5)`
#' * `eta_sdlog ~ normal(0, 0.5)`
#'
#' Note that the link function used for `meanlog` is a constant, and the link
#' function used for `sdlog` is a logarithm, such that the expectation of
#' `sdlog` is the exponential of `eta_sdlog`.
#'
#' To alter the priors for this model, we suggest two possible workflows:
#'
#' 1. Use `brms::get_prior` to extract the default `brms` priors for your model.
#' You may then alter these priors, and pass them to `epidist`. Note that the
#' default `brms` priors are different to the priors we suggest.
#'
#' 2. Alternatively, after fitting the model, the all priors used in the model
#' may be extracted using `brms::prior_summary()`.
#'
#' Examples of these two workflows will be provided as a part of a vignette at
#' a future date.
#'
#' @inheritParams epidist_prior
#' @method epidist_prior epidist_latent_individual
#' @family latent_individual
#' @export
epidist_prior.epidist_latent_individual <- function(data, ...) {
  prior1 <- brms::prior("normal(2, 0.5)", class = "Intercept")
  prior2 <- brms::prior("normal(0, 0.5)", class = "Intercept", dpar = "sigma")
  return(prior1 + prior2)
}

#' @method epidist_stancode epidist_latent_individual
#' @family latent_individual
#' @autoglobal
#' @export
epidist_stancode.epidist_latent_individual <- function(data,
                                                       family =
                                                         epidist_family(data),
                                                       ...) {

  stanvars_version <- epidist_version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = epidist_stan_chunk("latent_individual/functions.stan")
  )

  family_name <- gsub("latent_", "", family$name)

  stanvars_functions[[1]]$scode <- gsub(
    "family", family_name, stanvars_functions[[1]]$scode
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
    block = "parameters",
    scode = epidist_stan_chunk("latent_individual/parameters.stan")
  )

  stanvars_tparameters <- brms::stanvar(
    block = "tparameters",
    scode = epidist_stan_chunk("latent_individual/tparameters.stan")
  )

  stanvars_priors <- brms::stanvar(
    block = "model",
    scode = epidist_stan_chunk("latent_individual/priors.stan")
  )

  stanvars_all <- stanvars_version + stanvars_functions + stanvars_data +
    stanvars_parameters + stanvars_tparameters + stanvars_priors

  return(stanvars_all)
}
