#' Convert an object to an `epidist_marginal_model` object
#'
#' Creates an `epidist_marginal_model` object from various input formats.
#' This enables fitting marginal models for epidemiological delays using
#' [epidist()]. The marginal model approach uses the likelihood from the
#' [primarycensored](https://primarycensored.epinowcast.org/) package to
#' efficiently handle censoring in both primary and secondary events as well as
#' truncation due to observation times. See the specific methods
#' [as_epidist_marginal_model.epidist_linelist_data()] and
#' [as_epidist_marginal_model.epidist_aggregate_data()] for details on
#' supported input formats and usage examples.
#'
#' @param data An object to be converted to the class `epidist_marginal_model`
#'
#' @param ... Additional arguments passed to methods.
#'
#' @family marginal_model
#' @export
as_epidist_marginal_model <- function(data, ...) {
  UseMethod("as_epidist_marginal_model")
}

#' The marginal model method for `epidist_linelist_data` objects
#'
#' This method converts linelist data to a marginal model format by calculating
#' delays between primary and secondary events, along with observation times and
#' censoring windows. The likelihood used is imported from the
#' [primarycensored](https://primarycensored.epinowcast.org/) package
#' which handles censoring in both primary and secondary events as well as
#' truncation due to observation times. In principle, this method should be
#' more accurate and more computationally efficient than the latent model
#' ([as_epidist_latent_model()]) approach in most settings except when the
#' number of unique strata approaches the number of observations.
#'
#' When a formula is specified in [epidist()], the data will be transformed
#' using [epidist_transform_data_model.epidist_marginal_model()] to prepare it
#' for model fitting. This transformation summarises the data by counting unique
#' combinations of delays, observation times, censoring windows and any
#' variables in the model formula.
#'
#' @param data An `epidist_linelist_data` object
#'
#' @param obs_time_threshold Ratio used to determine threshold for setting
#'  relative observation times to Inf. Observation times greater than
#'  `obs_time_threshold` times the maximum delay will be set to Inf to improve
#'  model efficiency by reducing the number of unique observation times.
#'  Default is 2.
#'
#' @inheritParams .add_weights
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_marginal_model epidist_linelist_data
#' @family marginal_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_marginal_model()
as_epidist_marginal_model.epidist_linelist_data <- function(
  data,
  obs_time_threshold = 2,
  weight = NULL,
  ...
) {
  assert_epidist.epidist_linelist_data(data)

  data <- mutate(
    data,
    pwindow = .data$ptime_upr - .data$ptime_lwr,
    swindow = .data$stime_upr - .data$stime_lwr,
    relative_obs_time = .data$obs_time - .data$ptime_lwr,
    orig_relative_obs_time = .data$obs_time - .data$ptime_lwr,
    delay_lwr = .data$stime_lwr - .data$ptime_lwr,
    delay_upr = .data$stime_upr - .data$ptime_lwr
  )

  data <- .add_weights(data, weight)

  # Calculate maximum delay
  max_delay <- max(data$delay_upr, na.rm = TRUE)
  threshold <- max_delay * obs_time_threshold

  # Count observations beyond threshold
  n_beyond <- sum(data$relative_obs_time > threshold, na.rm = TRUE)

  if (n_beyond > 0) {
    cli::cli_inform(c(
      "!" = paste0(
        "Setting {n_beyond} observation time{?s} beyond ",
        "{threshold} (={obs_time_threshold}x max delay) to Inf. ",
        "This improves model efficiency by reducing unique observation times ",
        "while maintaining model accuracy as these times should have ",
        "negligible impact."
      )
    ))
    data$relative_obs_time[data$relative_obs_time > threshold] <- Inf
  }

  data <- new_epidist_marginal_model(data)
  assert_epidist(data)
  return(data)
}

#' The marginal model method for `epidist_aggregate_data` objects
#'
#' This method converts aggregate data to a marginal model format by
#' passing it to [as_epidist_marginal_model.epidist_linelist_data()]
#' with the `n` column used as weights. This ensures that the likelihood is
#' weighted by the counts in the aggregate data.
#'
#' @param data An `epidist_aggregate_data` object
#'
#' @inheritParams as_epidist_marginal_model.epidist_linelist_data
#'
#' @method as_epidist_marginal_model epidist_aggregate_data
#'
#' @family marginal_model
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
#'   as_epidist_marginal_model()
as_epidist_marginal_model.epidist_aggregate_data <- function(
  data,
  obs_time_threshold = 2,
  ...
) {
  return(as_epidist_marginal_model.epidist_linelist_data(
    data,
    obs_time_threshold = obs_time_threshold,
    weight = "n",
    ...
  ))
}

#' Class constructor for `epidist_marginal_model` objects
#'
#' @param data A data.frame to convert
#' @returns An object of class `epidist_marginal_model`
#' @family marginal_model
#' @export
new_epidist_marginal_model <- function(data) {
  class(data) <- c("epidist_marginal_model", class(data))
  return(data)
}

#' @method assert_epidist epidist_marginal_model
#' @family marginal_model
#' @export
assert_epidist.epidist_marginal_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = .marginal_required_cols())
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_integerish(data$delay_lwr)
  assert_integerish(data$delay_upr)
  assert_numeric(data$relative_obs_time)
  if (!all(abs(data$delay_upr - (data$delay_lwr + data$swindow)) < 1e-10)) {
    cli::cli_abort(
      "delay_upr must equal delay_lwr + swindow"
    )
  }
  if (!all(data$relative_obs_time >= data$delay_upr)) {
    cli::cli_abort(
      "relative_obs_time must be greater than or equal to delay_upr"
    )
  }
  assert_numeric(data$n, lower = 1)
  return(invisible(NULL))
}

#' Check if data has the `epidist_marginal_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family marginal_model
#' @export
is_epidist_marginal_model <- function(data) {
  return(inherits(data, "epidist_marginal_model"))
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_family_model epidist_marginal_model
#'
#' @family marginal_model
#' @export
epidist_family_model.epidist_marginal_model <- function(
  data,
  family,
  ...
) {
  custom_family <- brms::custom_family(
    paste0("marginal_", family$family),
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
    type = "int",
    vars = c(
      "vreal1[n]",
      "vreal2[n]",
      "vreal3[n]",
      "vreal4[n]",
      "primary_params"
    ),
    loop = TRUE,
    log_lik = epidist_gen_log_lik(family),
    posterior_predict = epidist_gen_posterior_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula for the
#' marginal model
#'
#' @inheritParams epidist_formula_model
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_formula_model epidist_marginal_model
#'
#' @family marginal_model
#' @export
epidist_formula_model.epidist_marginal_model <- function(
  data,
  formula,
  ...
) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula,
    delay_lwr |
      weights(n) +
        vreal(relative_obs_time, pwindow, swindow, delay_upr) ~
      .
  )
  return(formula)
}

#' Transform data for the marginal model
#'
#' This method transforms data into the format required by the marginal model
#' by:
#' 1. Identifying required columns for the marginal model
#' 2. Summarising the data by counting unique combinations of these columns and
#'    any variables in the model formula using [.summarise_n_by_formula()]
#' 3. Converting the summarised data to a marginal model object using
#'    [new_epidist_marginal_model()]
#' 4. Informing the user about any data aggregation that occurred using
#'    [.inform_data_summarised()]
#'
#' @param data The data to transform
#' @param family The epidist family object specifying the distribution
#' @param formula The model formula
#' @param ... Additional arguments passed to methods
#'
#' @method epidist_transform_data_model epidist_marginal_model
#' @family marginal_model
#' @importFrom purrr map_chr
#' @export
epidist_transform_data_model.epidist_marginal_model <- function(
  data,
  family,
  formula,
  ...
) {
  required_cols <- .marginal_required_cols()
  trans_data <- data |>
    .summarise_n_by_formula(by = required_cols, formula = formula) |>
    new_epidist_marginal_model()

  .inform_data_summarised(data, trans_data, c(required_cols))

  return(trans_data)
}

#' @method epidist_stancode epidist_marginal_model
#' @importFrom brms stanvar
#' @family marginal_model
#' @autoglobal
#' @export
epidist_stancode.epidist_marginal_model <- function(
  data,
  family = epidist_family(data),
  formula = epidist_formula(data),
  ...
) {
  assert_epidist.epidist_marginal_model(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("marginal_model", "functions.stan"))
  )

  family_name <- gsub("marginal_", "", family$name, fixed = TRUE)

  stanvars_functions[[1]]$scode <- gsub(
    "family",
    family_name,
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  dist_id <- primarycensored::pcd_stan_dist_id(family_name)

  # Replace the dist_id passed to primarycensored
  stanvars_functions[[1]]$scode <- gsub(
    "dist_id",
    dist_id,
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    toString(paste0("real ", family$dpars)),
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B",
    family$param,
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "primary_id",
    "1",
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_parameters <- brms::stanvar(
    block = "parameters",
    scode = "array[0] real primary_params;"
  )

  pcd_stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = primarycensored::pcd_load_stan_functions()
  )

  stanvars_all <- stanvars_version +
    stanvars_functions +
    pcd_stanvars_functions +
    stanvars_parameters

  return(stanvars_all)
}

.marginal_required_cols <- function() {
  return(c(
    "delay_lwr",
    "delay_upr",
    "relative_obs_time",
    "pwindow",
    "swindow",
    "n"
  ))
}
