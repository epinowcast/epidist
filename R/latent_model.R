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
    dpars = c(family$dpars, "pwindow", "swindow"),
    links = c(family$link, family$other_links, "identity", "identity"),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb")), 0, 0),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub")), 1, 1),
    type = family$type,
    vars = c("vreal1", "vreal2", "vreal3", "woverlap", "wN"),
    loop = FALSE,
    log_lik = epidist_gen_log_lik_latent(family),
    posterior_predict = epidist_gen_posterior_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  custom_family$reparm <- family$reparm
  return(custom_family)
}

#' Create a function to calculate the pointwise log likelihood of the latent
#' model
#'
#' This function creates a log likelihood function that accounts for the latent
#' variables in the model, including primary and secondary event windows and
#' their overlap. The returned function calculates the log likelihood for a
#' single observation by augmenting the data with the latent variables and
#' using the underlying brms log likelihood function.
#'
#' @seealso [brms::log_lik()] for details on the brms log likelihood interface.
#'
#' @inheritParams epidist_family
#'
#' @return A function that calculates the log likelihood for a single
#' observation. The prep object must have the following variables:
#' * `vreal1`: relative observation time
#' * `vreal2`: primary event window
#' * `vreal3`: secondary event window
#'
#' @family latent_model
#' @autoglobal
epidist_gen_log_lik_latent <- function(family) {
  # Get internal brms log_lik function
  log_lik_brms <- .get_brms_fn("log_lik", family)

  .log_lik <- function(i, prep) {
    y <- prep$data$Y[i]
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]

    pwindow_raw <- prep$dpars$pwindow[i, ]
    swindow_raw <- prep$dpars$swindow[i, ]

    swindow <- swindow_raw * swindow

    # For no overlap calculate as usual, for overlap ensure pwindow < swindow
    if (i %in% prep$data$woverlap) {
      pwindow <- pwindow_raw * pwindow
    } else {
      pwindow <- pwindow_raw * swindow
    }

    d <- y - pwindow + swindow
    obs_time <- relative_obs_time - pwindow
    # Create brms truncation upper bound
    prep$data$ub <- rep(obs_time, length(prep$data$Y))
    # Update augmented data
    prep$data$Y <- rep(d, length(prep$data$Y))

    # Call internal brms log_lik function with augmented data
    lpdf <- log_lik_brms(i, prep)
    return(lpdf)
  }

  return(.log_lik)
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

  # Only update pwindow/swindow formulas if intercept only
  fixed_dpars <- names(formula$pfix)
  formula_dpars <- names(formula$pforms)

  # Check if pwindow needs updating
  if (!("pwindow" %in% fixed_dpars) ||
    identical(formula_dpars$pwindow, as.formula("pwindow ~ 1"))) {
    formula$pforms$pwindow <- as.formula("pwindow ~ 0 + .row_id")
  }

  # Check if swindow needs updating
  if (!("swindow" %in% fixed_dpars) ||
    identical(formula_dpars$swindow, as.formula("swindow ~ 1"))) {
    formula$pforms$swindow <- as.formula("swindow ~ 0 + .row_id")
  }
  return(formula)
}

#' Model specific prior distributions for latent models
#'
#' Defines prior distributions for the latent model parameters `pwindow_raw` and
#' `swindow_raw` which control the width of the observation windows.
#'
#' @inheritParams epidist
#' @family latent_model
#' @export
epidist_model_prior.epidist_latent_model <- function(data, formula, ...) {
  priors <- c(
    prior(uniform(0, 1), class = "b", dpar = "pwindow", lb = 0, ub = 1),
    prior(uniform(0, 1), class = "b", dpar = "swindow", lb = 0, ub = 1)
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

  stanvars_all <- stanvars_version + stanvars_functions + stanvars_data

  return(stanvars_all)
}
