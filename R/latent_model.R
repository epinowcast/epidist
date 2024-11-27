#' Convert an object to an `epidist_latent_model` object
#'
#' @param data An object to be converted to the class `epidist_latent_model`
#' @family latent_model
#' @export
as_epidist_latent_model <- function(data) {
  UseMethod("as_epidist_latent_model")
}


#' The latent model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object
#' @method as_epidist_latent_model epidist_linelist_data
#' @family latent_model
#' @autoglobal
#' @export
as_epidist_latent_model.epidist_linelist_data <- function(data) {
  assert_epidist(data)
  data <- data |>
    mutate(
      relative_obs_time = .data$obs_time - .data$ptime_lwr,
      pwindow = ifelse(
        .data$stime_lwr < .data$ptime_upr,
        .data$stime_upr - .data$ptime_lwr,
        .data$ptime_upr - .data$ptime_lwr
      ),
      woverlap = as.numeric(.data$stime_lwr < .data$ptime_upr),
      swindow = .data$stime_upr - .data$stime_lwr,
      delay = .data$stime_lwr - .data$ptime_lwr,
      .row_id = as.character(dplyr::row_number())
    )
  data <- new_epidist_latent_model(data)
  assert_epidist(data)
  return(data)
}

#' Class constructor for `epidist_latent_model` objects
#'
#' @param data An object to be set with the class `epidist_latent_model`
#' @returns An object of class `epidist_latent_model`
#' @family latent_model
#' @export
new_epidist_latent_model <- function(data) {
  class(data) <- c("epidist_latent_model", class(data))
  return(data)
}

#' Check if data has the `epidist_latent_model` class
#'
#' @param data An object
#' @family latent_model
#' @export
is_epidist_latent_model <- function(data) {
  inherits(data, "epidist_latent_model")
}

#' @method assert_epidist epidist_latent_model
#' @family latent_model
#' @importFrom checkmate assert_names assert_numeric assert_character
#' @export
assert_epidist.epidist_latent_model <- function(data, ...) {
  col_names <- c(
    "ptime_lwr", "ptime_upr", "stime_lwr", "stime_upr", "obs_time",
    "relative_obs_time", "pwindow", "woverlap", "swindow", "delay", ".row_id"
  )
  assert_names(names(data), must.include = col_names)
  assert_numeric(data$relative_obs_time, lower = 0)
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$woverlap, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_numeric(data$delay, lower = 0)
  assert_character(data$.row_id)
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#' @param ... Additional arguments passed to method.
#' @method epidist_family_model epidist_latent_model
#' @family latent_model
#' @export
epidist_family_model.epidist_latent_model <- function(
    data, family, ...) {
  # Really the name and vars are the "model-specific" parts here
  custom_family <- brms::custom_family(
    paste0("latent_", family$family),
    dpars = c(family$dpar),
    links = c(family$link, family$other_links),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub"))),
    type = family$type,
    vars = c(
      "vreal1", "vreal2", "vreal3", "pwindow_raw", "swindow_raw",
      "woverlap", "wN"
    ),
    loop = FALSE,
    log_lik = epidist_gen_log_lik(family),
    posterior_predict = epidist_gen_posterior_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  custom_family$reparm <- family$reparm
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula
#'
#' @inheritParams epidist_formula_model
#' @param ... Additional arguments passed to method.
#' @method epidist_formula_model epidist_latent_model
#' @family latent_model
#' @export
epidist_formula_model.epidist_latent_model <- function(
    data, formula, ...) {
  # Update main formula
  formula <- stats::update(
    formula, delay | vreal(relative_obs_time, pwindow, swindow) ~ .
  )
  return(formula)
}

#' Model specific prior distributions for latent models
#'
#' Defines prior distributions for the latent model parameters `pwindow_raw` and
#' `swindow_raw` which control the width of the observation windows.
#'
#' @inheritParams epidist
#' @importFrom brms set_prior
#' @family latent_model
#' @export
epidist_model_prior.epidist_latent_model <- function(data, formula, ...) {
  priors <- prior(
    "pwindow_raw ~ uniform(0, 1);",
    dpar = "pwindow_raw",
    check = FALSE
  ) +
    prior(
      "swindow_raw ~ uniform(0, 1);",
      dpar = "swindow_raw",
      check = FALSE
    )
  return(priors)
}

#' @method epidist_stancode epidist_latent_model
#' @importFrom brms stanvar
#' @family latent_model
#' @autoglobal
#' @export
epidist_stancode.epidist_latent_model <- function(
    data,
    family = epidist_family(data),
    formula = epidist_formula(data), ...) {
  assert_epidist(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("latent_model", "functions.stan"))
  )

  family_name <- gsub("latent_", "", family$name, fixed = TRUE)

  stanvars_functions[[1]]$scode <- gsub(
    "family", family_name, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  # Inject vector or real depending if there is a model for each dpar
  vector_real <- purrr::map_vec(family$dpars, function(dpar) {
    ifelse(dpar %in% c("mu", names(formula$pforms)), "vector", "real")
  })

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    toString(paste0(vector_real, " ", family$dpars)),
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B", family$param, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_data <- stanvar(
    block = "data",
    scode = "int wN;",
    x = nrow(filter(data, woverlap > 0)),
    name = "wN"
  ) +
    stanvar(
      block = "data",
      scode = "array[wN] int woverlap;",
      x = as.integer(filter(data, woverlap > 0)$.row_id),
      name = "woverlap"
    )

  stanvars_parameters <- stanvar(
    block = "parameters",
    scode = "vector<lower=0,upper=1>[N] pwindow_raw;"
  ) +
    stanvar(
      block = "parameters",
      scode = "vector<lower=0,upper=1>[N] swindow_raw;"
    )

  stanvars_all <- stanvars_version + stanvars_functions + stanvars_data +
    stanvars_parameters

  return(stanvars_all)
}
