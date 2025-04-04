#' Convert an object to an `epidist_latent_model` object
#'
#' Creates an `epidist_latent_model` object from various input formats.
#' This enables fitting latent variable models for epidemiological delays using
#' [epidist()], as described in Park et al. (2024) and Charniga et al. (2024)
#' The latent model approach accounts for double interval censoring and right
#' truncation in delay data.
#'
#' @param data An object to be converted to  the class `epidist_latent_model`
#'
#' @param ... Additional arguments passed to methods.
#' @references
#'   - [Park et al. (2024)](https://doi.org/10.1101/2024.01.12.24301247)
#'   - [Charniga et al. (2024)](https://doi.org/10.1371/journal.pcbi.1012520)
#' @family latent_model
#' @export
as_epidist_latent_model <- function(data, ...) {
  UseMethod("as_epidist_latent_model")
}


#' The latent model method for `epidist_linelist_data` objects
#'
#' This method takes an `epidist_linelist_data` object and converts it to a
#'  format suitable for fitting latent variable models. It calculates key
#'  variables needed for the latent variable method described in Park et al.
#'  (2024) and Charniga et al. (2024). This approach adjusts for double
#'  interval censoring and right truncation in the data.
#'
#' @param data An `epidist_linelist_data` object containing individual-level
#'   observations with primary and secondary event times. See
#'   [as_epidist_linelist_data()] for details on creating this object.
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_latent_model epidist_linelist_data
#' @family latent_model
#' @autoglobal
#' @export
#' @references
#'   - [Park et al. (2024)](https://doi.org/10.1101/2024.01.12.24301247)
#'   - [Charniga et al. (2024)](https://doi.org/10.1371/journal.pcbi.1012520)
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_latent_model()
as_epidist_latent_model.epidist_linelist_data <- function(data, ...) {
  assert_epidist(data)
  data <- data |>
    mutate(
      # Time since primary event to observation
      relative_obs_time = .data$obs_time - .data$ptime_lwr,
      # Primary event window accounting for overlap with secondary event
      pwindow = ifelse(
        .data$stime_lwr < .data$ptime_upr,
        .data$stime_upr - .data$ptime_lwr,
        .data$ptime_upr - .data$ptime_lwr
      ),
      # Indicator for overlapping primary and secondary windows
      woverlap = as.numeric(.data$stime_lwr < .data$ptime_upr),
      # Secondary event window size
      swindow = .data$stime_upr - .data$stime_lwr,
      # Delay between primary and secondary events
      delay = .data$stime_lwr - .data$ptime_lwr,
      .row_id = dplyr::row_number()
    )
  data <- new_epidist_latent_model(data)
  assert_epidist(data)
  return(data)
}

#' The latent model method for `epidist_aggregate_data` objects
#'
#' This method converts aggregate data to a latent model format by first
#' converting it to linelist format using
#' [as_epidist_linelist_data.epidist_aggregate_data()] and then passing it to
#' [as_epidist_latent_model.epidist_linelist_data()]. This ensures that the
#' counts in the aggregate data are properly expanded into individual
#' observations before fitting the latent model.
#'
#' @param data An `epidist_aggregate_data` object
#' @param ... Not used in this method.
#' @method as_epidist_latent_model epidist_aggregate_data
#' @family latent_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
#'   as_epidist_aggregate_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested",
#'     n = "n"
#'   ) |>
#'   as_epidist_latent_model()
as_epidist_latent_model.epidist_aggregate_data <- function(data, ...) {
  linelist_data <- as_epidist_linelist_data.epidist_aggregate_data(data)
  return(as_epidist_latent_model(linelist_data))
}

#' Class constructor for `epidist_latent_model` objects
#'
#' @param data An object to be set with the class `epidist_latent_model`
#'
#' @param ... Additional arguments passed to methods.
#'
#' @returns An object of class `epidist_latent_model`
#'
#' @family latent_model
#' @export
new_epidist_latent_model <- function(data, ...) {
  class(data) <- c("epidist_latent_model", class(data))
  return(data)
}

#' Check if data has the `epidist_latent_model` class
#'
#' @param data An object
#'
#' @family latent_model
#' @export
is_epidist_latent_model <- function(data) {
  return(inherits(data, "epidist_latent_model"))
}

#' @method assert_epidist epidist_latent_model
#' @family latent_model
#' @importFrom checkmate assert_names assert_numeric assert_integerish
#' @export
assert_epidist.epidist_latent_model <- function(data, ...) {
  assert_names(names(data), must.include = .latent_required_cols())
  assert_numeric(data$relative_obs_time, lower = 0)
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$woverlap, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_numeric(data$delay, lower = 0)
  assert_integerish(data$.row_id, lower = 1)
  return(invisible(NULL))
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_family_model epidist_latent_model
#'
#' @family latent_model
#' @export
epidist_family_model.epidist_latent_model <- function(
    data,
    family,
    ...) {
  # Really the name and vars are the "model-specific" parts here
  custom_family <- brms::custom_family(
    paste0("latent_", family$family),
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(
      as.numeric(family$ybounds[1]),
      as.numeric(lapply(family$other_bounds, "[[", "lb"))
    ),
    ub = c(
      as.numeric(family$ybounds[2]),
      as.numeric(lapply(family$other_bounds, "[[", "ub"))
    ),
    type = family$type,
    vars = c(
      "vreal1",
      "vreal2",
      "vreal3",
      "pwindow_raw",
      "swindow_raw",
      "woverlap",
      "wN"
    ),
    loop = FALSE,
    log_lik = epidist_gen_log_lik(family),
    posterior_predict = epidist_gen_posterior_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  custom_family$reparm <- family$reparm
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula for the
#' latent model
#'
#' @inheritParams epidist_formula_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_formula_model epidist_latent_model
#'
#' @family latent_model
#' @export
epidist_formula_model.epidist_latent_model <- function(
    data,
    formula,
    ...) {
  formula <- stats::update(
    formula,
    delay | vreal(relative_obs_time, pwindow, swindow) ~ .
  )
  return(formula)
}

#' Model specific prior distributions for latent models
#'
#' Defines prior distributions for the latent model parameters `pwindow_raw` and
#' `swindow_raw` which control the width of the observation windows.
#'
#' @inheritParams epidist
#'
#' @importFrom brms set_prior
#'
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
#'
#' @importFrom brms stanvar
#'
#' @family latent_model
#' @autoglobal
#' @export
epidist_stancode.epidist_latent_model <- function(
    data,
    family = epidist_family(data),
    formula = epidist_formula(data),
    ...) {
  assert_epidist(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("latent_model", "functions.stan"))
  )

  family_name <- gsub("latent_", "", family$name, fixed = TRUE)

  stanvars_functions[[1]]$scode <- gsub(
    "family",
    family_name,
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  # Inject vector or real depending if there is a model for each dpar
  vector_real <- purrr::map_vec(family$dpars, function(dpar) {
    return(ifelse(dpar %in% c("mu", names(formula$pforms)), "vector", "real"))
  })

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    toString(paste0(vector_real, " ", family$dpars)),
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B",
    family$param,
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_data <- stanvar(
    block = "data",
    scode = "int wN;",
    x = sum(data$woverlap > 0, na.rm = TRUE),
    name = "wN"
  ) +
    stanvar(
      block = "data",
      scode = "array[wN] int woverlap;",
      x = filter(data, woverlap > 0)$.row_id,
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

  stanvars_all <- stanvars_version +
    stanvars_functions +
    stanvars_data +
    stanvars_parameters

  return(stanvars_all)
}

.latent_required_cols <- function() {
  return(c(
    .linelist_required_cols(),
    "relative_obs_time",
    "pwindow",
    "woverlap",
    "swindow",
    "delay",
    ".row_id"
  ))
}
