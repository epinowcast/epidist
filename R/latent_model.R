#' Prepare latent model
#'
#' @param data A `data.frame` containing line list data
#' @family latent_model
#' @export
as_latent_model <- function(data) {
  UseMethod("as_latent_model")
}

#' @method as_latent_model epidist_linelist
#' @family latent_model
#' @autoglobal
#' @export
as_latent_model.epidist_linelist <- function(data) {
  epidist_validate_data(data)
  class(data) <- c("epidist_latent_model", class(data))
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
      .row_id = dplyr::row_number()
    )
  epidist_validate_model(data)
  return(data)
}

#' @method epidist_validate_model epidist_latent_model
#' @family latent_model
#' @export
epidist_validate_model.epidist_latent_model <- function(data, ...) {
  assert_true(is_latent_model(data))
  col_names <- c(
    "ptime_lwr", "ptime_upr", "stime_lwr", "stime_upr", "obs_time",
    "relative_obs_time", "pwindow", "woverlap", "swindow", "delay", ".row_id"
  )
  assert_names(names(data), must.include = col_names)
  assert_numeric(data$relative_obs_time, lower = 0)
  # pwindow as f(p) and swindow as f(s) checks here?
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$woverlap, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_numeric(data$delay, lower = 0)
}

#' Check if data has the `epidist_latent_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family latent_model
#' @export
is_latent_model <- function(data) {
  inherits(data, "epidist_latent_model")
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#' @param ... Additional arguments passed to method.
#' @method epidist_family_model epidist_latent_model
#' @family latent_model
#' @export
epidist_family_model.epidist_latent_model <- function(
  data, family, ...
) {
  # Really the name and vars are the "model-specific" parts here
  custom_family <- brms::custom_family(
    paste0("latent_", family$family),
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub"))),
    type = family$type,
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  )
  custom_family$reparm <- family$reparm
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula
#'
#' @param data A `data.frame` containing line list data
#' @param formula As produced by [brms::brmsformula()]
#' @param ... ...
#' @method epidist_formula_model epidist_latent_model
#' @family latent_model
#' @export
epidist_formula_model.epidist_latent_model <- function(
  data, formula, ...
) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula, delay | vreal(relative_obs_time, pwindow, swindow) ~ .
  )
  return(formula)
}

#' @method epidist_stancode epidist_latent_model
#' @family latent_model
#' @autoglobal
#' @export
epidist_stancode.epidist_latent_model <- function(data,
                                                       family =
                                                         epidist_family(data),
                                                       formula =
                                                         epidist_formula(data),
                                                       ...) {
  epidist_validate_model(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk("latent_model/functions.stan")
  )

  family_name <- gsub("latent_", "", family$name)

  stanvars_functions[[1]]$scode <- gsub(
    "family", family_name, stanvars_functions[[1]]$scode
  )

  # Inject vector or real depending if there is a model for each dpar
  vector_real <- purrr::map_vec(family$dpars, function(dpar) {
    ifelse(dpar %in% c("mu", names(formula$pforms)), "vector", "real")
  })

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    paste(paste0(vector_real, " ", family$dpars), collapse = ", "),
    stanvars_functions[[1]]$scode
  )

  # dpars_B refers to the insertion into the lpdf call
  # For some families, we tranform brms dpars to match Stan parameterisation
  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B",
    paste(family$reparam, collapse = ", "),
    stanvars_functions[[1]]$scode
  )

  stanvars_data <- brms::stanvar(
    block = "data",
    scode = "int wN;",
    x = nrow(filter(data, woverlap > 0)),
    name = "wN"
  ) +
    brms::stanvar(
      block = "data",
      scode = "array[N - wN] int noverlap;",
      x = filter(data, woverlap == 0)$.row_id,
      name = "noverlap"
    ) +
    brms::stanvar(
      block = "data",
      scode = "array[wN] int woverlap;",
      x = filter(data, woverlap > 0)$.row_id,
      name = "woverlap"
    )

  stanvars_parameters <- brms::stanvar(
    block = "parameters",
    scode = .stan_chunk("latent_model/parameters.stan")
  )

  stanvars_tparameters <- brms::stanvar(
    block = "tparameters",
    scode = .stan_chunk("latent_model/tparameters.stan")
  )

  stanvars_priors <- brms::stanvar(
    block = "model",
    scode = .stan_chunk("latent_model/priors.stan")
  )

  stanvars_all <- stanvars_version + stanvars_functions + stanvars_data +
    stanvars_parameters + stanvars_tparameters + stanvars_priors

  return(stanvars_all)
}
