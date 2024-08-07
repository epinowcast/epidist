#' Prepare latent individual model
#'
#' @param data Input data to be used for modelling.
#' @family latent_individual
#' @export
as_latent_individual <- function(data) {
  UseMethod("as_latent_individual")
}

assert_latent_individual_input <- function(data) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(
    names(data),
    must.include = c("case", "ptime_lwr", "ptime_upr",
                     "stime_lwr", "stime_upr", "obs_at")
  )
  checkmate::assert_integer(data$case, lower = 0)
  checkmate::assert_numeric(data$ptime_lwr, lower = 0)
  checkmate::assert_numeric(data$ptime_upr, lower = 0)
  checkmate::assert_true(all(data$ptime_upr - data$ptime_lwr > 0))
  checkmate::assert_numeric(data$stime_lwr, lower = 0)
  checkmate::assert_numeric(data$stime_upr, lower = 0)
  checkmate::assert_true(all(data$stime_upr - data$stime_lwr > 0))
  checkmate::assert_numeric(data$obs_at, lower = 0)
}

#' Prepare latent individual model
#'
#' This function prepares data for use with the latent individual model. It does
#' this by adding columns used in the model to the `data` object provided. To do
#' this, the `data` must already have columns for the case number (integer),
#' (positive, numeric) upper and lower bounds for the primary and secondary
#' event times, as well as a (positive, numeric) time that observation takes
#' place. The output of this function is a `epidist_latent_individual` class
#' object, which may be passed to `epidist()` to perform inference for the
#' model.
#'
#' @param data A `data.frame` or `data.table` containing line list data
#' @rdname as_latent_individual
#' @method as_latent_individual data.frame
#' @family latent_individual
#' @importFrom checkmate assert_data_frame assert_names assert_int
#' assert_numeric
#' @autoglobal
#' @export
as_latent_individual.data.frame <- function(data) {
  assert_latent_individual_input(data)
  class(data) <- c(class(data), "epidist_latent_individual")
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
  epidist_validate(data)
  return(data)
}

#' Validate latent individual model data
#'
#' This function checks whether the provided `data` object is suitable for
#' running the latent individual model. As well as making sure that
#' `is_latent_individual()` is true, it also checks that `data` is a
#' `data.frame` with the correct columns.
#'
#' @param data A `data.frame` or `data.table` containing line list data
#' @importFrom checkmate assert_data_frame assert_names assert_int
#' assert_numeric
#' @method epidist_validate epidist_latent_individual
#' @family latent_individual
#' @export
epidist_validate.epidist_latent_individual <- function(data) {
  checkmate::assert_true(is_latent_individual(data))
  assert_latent_individual_input(data)
  checkmate::assert_names(
    names(data),
    must.include = c("case", "ptime_lwr", "ptime_upr",
                     "stime_lwr", "stime_upr", "obs_at",
                     "id", "obs_t", "pwindow_upr", "woverlap",
                     "swindow_upr", "delay_central", "row_id")
  )
  if (nrow(data) > 1) {
    checkmate::assert_factor(data$id)
  }
  checkmate::assert_numeric(data$obs_t, lower = 0)
  checkmate::assert_numeric(data$pwindow_upr, lower = 0)
  checkmate::assert_numeric(data$woverlap, lower = 0)
  checkmate::assert_numeric(data$swindow_upr, lower = 0)
  checkmate::assert_numeric(data$delay_central, lower = 0)
  checkmate::assert_integer(data$row_id, lower = 0)
}

#' Check if data has the `epidist_latent_individual` class
#'
#' @param data A `data.frame` or `data.table` containing line list data
#' @family latent_individual
#' @export
is_latent_individual <- function(data) {
  inherits(data, "epidist_latent_individual")
}

#' Check if data has the `epidist_latent_individual` class
#'
#' @param data A `data.frame` or `data.table` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param ... ...
#'
#' @importFrom rstan lookup
#' @method epidist_family epidist_latent_individual
#' @family latent_individual
#' @export
epidist_family.epidist_latent_individual <- function(data,
                                                     family = brms::lognormal(),
                                                     ...) {
  epidist_validate(data)
  # allows use of stats::family and strings
  family <- brms:::validate_family(family)
  non_mu_links <- family[[paste0("link_", setdiff(family$dpars, "mu"))]]
  non_mu_bounds <- lapply(
    family$dpars[-1], brms:::dpar_bounds, family = family$family
  )
  brms::custom_family(
    paste0("latent_", family$family),
    dpars = family$dpars,
    links = c(family$link, non_mu_links),
    lb = c(NA, as.numeric(lapply(non_mu_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(non_mu_bounds, "[[", "ub"))),
    type = family$type,
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  )
}

#' Define a formula for the latent_individual model
#'
#' @param data ...
#' @param family The output of `epidist_family()`
#' @param formula As produced by `brms::brmsformula`
#' @param ... ...
#' @method epidist_formula epidist_latent_individual
#' @family latent_individual
#' @importFrom brms brmsterms
#' @importFrom stats update
#' @export
epidist_formula.epidist_latent_individual <- function(data, family, formula,
                                                      ...) {
  epidist_validate(data)
  formula <- brms:::validate_formula(formula, data = data, family = family)

  formula <- stats::update(
    formula, delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ .
  )

  # Using this here for checking purposes
  bterms <- brms::brmsterms(formula)
  brms:::validate_data(data, bterms)

  return(formula)
}

#' Define priors for the model
#'
#' We provide suggested weakly informative priors.
#'
#' @inheritParams epidist_prior
#' @method epidist_prior epidist_latent_individual
#' @family latent_individual
#' @importFrom cli cli_inform
#' @export
epidist_prior.epidist_latent_individual <- function(data, family, formula,
                                                    ...) {
  epidist_validate(data)
  if (identical(family$dpars, c("mu", "sigma"))) {
    prior_mu <- brms::prior("normal(2, 0.5)", class = "Intercept")
    prior_sigma <- brms::prior(
      "normal(0, 0.5)", class = "Intercept", dpar = "sigma"
    )
    msg <- c(
      "i" = "The following priors have been set:",
      "*" = "normal(2, 0.5) on the intercept of distributional parameter mu",
      "*" = "normal(0, 0.5) on the intercept of distributional parameter sigma",
      "To alter priors, or set priors on other parameters, see ?epidist_prior."
    )
    cli::cli_inform(
      message = msg, .frequency = "regularly", .frequency_id = "prior-message"
    )
    priors <- prior_mu + prior_sigma
  } else {
    cli::cli_warn(c(
      "!" = "Priors not available for these distributional parameters.",
      "Using the default priors from brms."
    ))
    priors <- NULL
  }
  return(priors)
}

#' @method epidist_stancode epidist_latent_individual
#' @family latent_individual
#' @autoglobal
#' @export
epidist_stancode.epidist_latent_individual <- function(data,
                                                       family =
                                                         epidist_family(data),
                                                       ...) {

  epidist_validate(data)

  stanvars_version <- epidist_version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = epidist_stan_chunk("latent_individual/functions.stan")
  )

  family_name <- gsub("latent_", "", family$name)

  stanvars_functions[[1]]$scode <- gsub(
    "family", family_name, stanvars_functions[[1]]$scode
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    paste(paste0("vector ", family$dpars), collapse = ", "),
    stanvars_functions[[1]]$scode
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B",
    paste(family$dpars, collapse = ", "),
    stanvars_functions[[1]]$scode
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
