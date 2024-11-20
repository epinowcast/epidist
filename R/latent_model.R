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
      .row_id = dplyr::row_number()
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
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub"))),
    type = family$type,
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE,
    log_lik = log_lik_latent,
    posterior_predict = posterior_predict_latent(family),
    posterior_epred = posterior_epred_latent(family)
  )
  custom_family$reparm <- family$reparm
  return(custom_family)
}

#' Calculate the pointwise log likelihood of the `latent_gamma` family
#'
#' See [brms::log_lik()].
#'
#' @param i The index of the observation to calculate the log likelihood of
#' @param prep The result of a call to [brms::prepare_predictions()]
#' @autoglobal
#' @keywords internal
log_lik_latent <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  y <- prep$data$Y[i]
  relative_obs_time <- prep$data$vreal1[i]
  pwindow <- prep$data$vreal2[i]
  swindow <- prep$data$vreal3[i]

  # Generates values of the swindow_raw and pwindow_raw, but really these should
  # be extracted from prep or the fitted raws somehow. See:
  # https://github.com/epinowcast/epidist/issues/267
  swindow_raw <- stats::runif(prep$ndraws)
  pwindow_raw <- stats::runif(prep$ndraws)

  swindow <- swindow_raw * swindow

  # For no overlap calculate as usual, for overlap ensure pwindow < swindow
  if (i %in% prep$data$noverlap) {
    pwindow <- pwindow_raw * pwindow
  } else {
    pwindow <- pwindow_raw * swindow
  }

  d <- y - pwindow + swindow
  obs_time <- relative_obs_time - pwindow
  lpdf <- stats::dlnorm(d, meanlog = mu, sdlog = sigma, log = TRUE)
  lcdf <- stats::plnorm(obs_time, meanlog = mu, sdlog = sigma, log.p = TRUE)
  return(lpdf - lcdf)
}

#' Create a function to draw from the posterior predictive distribution for a
#' latent model
#'
#' This function creates a function that draws from the posterior predictive
#' distribution for a latent model using [primarycensored::rpcens()] to handle
#' censoring and truncation. The returned function takes a prep argument from
#' `brms` and returns posterior predictions. This is used internally by
#' [brms::posterior_predict()] to generate predictions for latent models.
#'
#' @inheritParams epidist_family_model
#'
#' @return A function that takes a prep argument from brms and returns a matrix
#' of posterior predictions, with one row per posterior draw and one column
#' per observation.
#'
#' @seealso [brms::posterior_predict()] for details on how this is used within
#' `brms`, [primarycensored::rpcens()] for details on the censoring approach
#' @autoglobal
#' @family latent_model
#' @export
posterior_predict_latent <- function(family) {
  fn_string <- paste0("brms:::posterior_predict_", family$family)
  dist_fn <- eval(parse(text = fn_string))

  rdist <- function(n, i, prep, ...) {
    purrr::map_dbl(
      seq_len(n),
      ~ do.call(dist_fn, list(i = i, prep = prep))
    )
  }

  .predict <- function(i, prep, ...) {
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]

    primarycensored::rpcens(
      n = prep$ndraws,
      rdist = rdist,
      rprimary = stats::runif,
      pwindow = prep$data$vreal2[i],
      swindow = prep$data$vreal3[i],
      D = prep$data$vreal1[i],
      i = i,
      prep = prep
    )
  }
  return(.predict)
}

#' Create a function to draw from the expected value of the posterior predictive
#' distribution for a latent model
#'
#' This function creates a function that calculates the expected value of the
#' posterior predictive distribution for a latent model. The returned function
#' takes a prep argument (from brms) and returns posterior expected values.
#' This is used internally by [brms::posterior_epred()] to calculate expected
#' values for latent models.
#'
#' @inheritParams epidist_family_model
#'
#' @return A function that takes a prep argument from brms and returns a matrix
#' of posterior expected values, with one row per posterior draw and one column
#' per observation.
#'
#' @seealso [brms::posterior_epred()] for details on how this is used within
#' `brms`.
#' @autoglobal
#' @family latent_model
#' @export
posterior_epred_latent <- function(family) {
  fn_string <- paste0("brms:::posterior_epred_", family$family)
  eval(parse(text = fn_string))
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
  # data is only used to dispatch on
  formula <- stats::update(
    formula, delay | vreal(relative_obs_time, pwindow, swindow) ~ .
  )
  return(formula)
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

  # dpars_B refers to the insertion into the lpdf call
  # For some families, we tranform brms dpars to match Stan parameterisation
  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B",
    toString(family$reparam),
    stanvars_functions[[1]]$scode,
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
      scode = "array[N - wN] int noverlap;",
      x = filter(data, woverlap == 0)$.row_id,
      name = "noverlap"
    ) +
    stanvar(
      block = "data",
      scode = "array[wN] int woverlap;",
      x = filter(data, woverlap > 0)$.row_id,
      name = "woverlap"
    )

  stanvars_parameters <- stanvar(
    block = "parameters",
    scode = .stan_chunk(file.path("latent_model", "parameters.stan"))
  )

  stanvars_tparameters <- stanvar(
    block = "tparameters",
    scode = .stan_chunk(file.path("latent_model", "tparameters.stan"))
  )

  stanvars_priors <- stanvar(
    block = "model",
    scode = .stan_chunk(file.path("latent_model", "priors.stan"))
  )

  stanvars_all <- stanvars_version + stanvars_functions + stanvars_data +
    stanvars_parameters + stanvars_tparameters + stanvars_priors

  return(stanvars_all)
}
