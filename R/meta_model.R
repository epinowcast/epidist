#' Convert an object to an `epidist_meta_model` object
#'
#' Creates an `epidist_meta_model` object from individual level data, published
#' summary estimates, or a mix of the two.
#' This enables fitting a single delay distribution to all of the evidence
#' available using [epidist()].
#'
#' Individual level rows use the same likelihood as the marginal model (see
#' [as_epidist_marginal_model()]), imported from the
#' [primarycensored](https://primarycensored.epinowcast.org/) package.
#' Summary rows instead forward model what the study that reported them would
#' have converged to given its estimation procedure, and fit the reported value
#' to that with sampling uncertainty derived from the study sample size. This
#' means published estimates that did not adjust for right truncation, or that
#' treated interval censored data as continuous, still contribute unbiased
#' information about the underlying delay distribution.
#'
#' At least one of `data` and `estimates` must be supplied. Study level
#' heterogeneity is specified through the `brms` formula in [epidist()], for
#' example `mu ~ 1 + (1 | study)`, rather than through this function.
#' Individual level rows are labelled `"individual"` in the `study` column so
#' that they form their own level of any such term.
#'
#' # Approximations used for summary rows
#'
#' * Reported means, standard deviations and quantiles are given normal
#'   sampling distributions. A reported mean uses the implied standard
#'   deviation over the square root of the sample size, a reported standard
#'   deviation uses the kurtosis based asymptotic standard error of a sample
#'   standard deviation, and a reported quantile is fitted on the cumulative
#'   probability scale with the binomial standard error of an empirical
#'   distribution function. These approximations degrade for small sample
#'   sizes.
#' * Several summaries reported by the same study are treated as independent
#'   given the parameters, which understates their joint uncertainty.
#' * The standard errors above are plug in quantities that depend on the
#'   parameters, so a set of studies that no single distribution can explain
#'   can be accommodated by inflating the implied standard deviation rather
#'   than by moving the location, and sampling can then become multimodal.
#'   Allow for genuine differences between studies with a formula term such as
#'   `mu ~ 1 + (1 | study)` rather than relying on the sampling error alone.
#' * Quantiles read off a fitted distribution rather than the empirical data
#'   have smaller sampling error than assumed here. Supply a reported `se` in
#'   [as_epidist_estimates_data()] for those rows. For quantile rows that `se`
#'   is interpreted on the cumulative probability scale, not on the scale of
#'   the reported delay.
#' * A study that took integer date differences reports quantiles of a
#'   discrete distribution, so the implied cumulative probability is
#'   continuity corrected by interpolating the grid distribution function
#'   through the mid points of its cells. A small discretisation bias remains
#'   that does not shrink with the study sample size.
#' * Studies that adjusted for right truncation but not for censoring are
#'   summarised on a grid running to `max_delay`, so a distribution with a
#'   long tail needs a larger `max_delay` than the default.
#' * Studies that used the uniform single interval approximation and did not
#'   adjust for right truncation are summarised by quadrature over the
#'   primary censored delay distribution, which costs a fixed number of
#'   distribution function evaluations per row.
#'
#' @param data An `epidist_linelist_data` or `epidist_aggregate_data` object of
#'  individual level observations, an `epidist_estimates_data` object of
#'  published summary estimates, or `NULL`.
#'
#' @param estimates An `epidist_estimates_data` object of published summary
#'  estimates, or `NULL`.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @family meta_model
#' @export
as_epidist_meta_model <- function(data = NULL, estimates = NULL, ...) {
  # Dispatch explicitly on data so that a summary only call, where data is
  # missing, reaches the NULL method rather than the estimates method.
  UseMethod("as_epidist_meta_model", data)
}

#' The meta model method for `epidist_linelist_data` objects
#'
#' Prepares individual level data exactly as
#' [as_epidist_marginal_model.epidist_linelist_data()] does and then stacks it
#' with any supplied summary estimates.
#'
#' @param data An `epidist_linelist_data` object.
#'
#' @inheritParams as_epidist_meta_model
#' @inheritParams as_epidist_marginal_model.epidist_linelist_data
#'
#' @method as_epidist_meta_model epidist_linelist_data
#'
#' @family meta_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_meta_model()
as_epidist_meta_model.epidist_linelist_data <- function(
  data,
  estimates = NULL,
  obs_time_threshold = 2,
  weight = NULL,
  ...
) {
  assert_epidist.epidist_linelist_data(data)
  data <- .prepare_marginal_data(
    data,
    obs_time_threshold = obs_time_threshold,
    weight = weight
  )
  return(.new_meta_model_from_parts(data, estimates))
}

#' The meta model method for `epidist_aggregate_data` objects
#'
#' This method converts aggregate data to a meta model format by passing it to
#' [as_epidist_meta_model.epidist_linelist_data()] with the `n` column used as
#' weights.
#'
#' @param data An `epidist_aggregate_data` object.
#'
#' @inheritParams as_epidist_meta_model.epidist_linelist_data
#'
#' @method as_epidist_meta_model epidist_aggregate_data
#'
#' @family meta_model
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
#'   as_epidist_meta_model()
as_epidist_meta_model.epidist_aggregate_data <- function(
  data,
  estimates = NULL,
  obs_time_threshold = 2,
  ...
) {
  return(as_epidist_meta_model.epidist_linelist_data(
    data,
    estimates = estimates,
    obs_time_threshold = obs_time_threshold,
    weight = "n",
    ...
  ))
}

#' The meta model method for `epidist_estimates_data` objects
#'
#' Allows summary estimates to be passed as the first argument, so that
#' `as_epidist_meta_model(estimates)` and
#' `as_epidist_meta_model(estimates = estimates)` are equivalent.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @inheritParams as_epidist_meta_model
#'
#' @method as_epidist_meta_model epidist_estimates_data
#'
#' @family meta_model
#' @export
as_epidist_meta_model.epidist_estimates_data <- function(
  data,
  estimates = NULL,
  ...
) {
  if (!is.null(estimates)) {
    cli::cli_abort(paste0(
      "Summary estimates were supplied twice. Pass individual level data as ",
      "{.var data} and summary estimates as {.var estimates}."
    ))
  }
  return(.new_meta_model_from_parts(NULL, data))
}

#' The meta model method for summary estimates only
#'
#' Used when no individual level data is available and only the `estimates`
#' argument is supplied.
#'
#' @param data `NULL`.
#'
#' @inheritParams as_epidist_meta_model
#'
#' @method as_epidist_meta_model NULL
#'
#' @family meta_model
#' @export
as_epidist_meta_model.NULL <- function(data = NULL, estimates = NULL, ...) {
  return(.new_meta_model_from_parts(NULL, estimates))
}

#' Build an `epidist_meta_model` object from its individual and summary parts
#'
#' @param data Individual level data prepared by [.prepare_marginal_data()], or
#'  `NULL`.
#'
#' @param estimates An `epidist_estimates_data` object, or `NULL`.
#'
#' @returns An object of class `epidist_meta_model`.
#'
#' @keywords internal
#' @autoglobal
#' @importFrom dplyr bind_rows
.new_meta_model_from_parts <- function(data, estimates) {
  if (is.null(data) && is.null(estimates)) {
    cli::cli_abort(paste0(
      "The meta model needs at least one of individual level {.var data} and ",
      "summary {.var estimates}."
    ))
  }
  individual_rows <- NULL
  if (!is.null(data)) {
    individual_rows <- .meta_individual_rows(data)
  }
  estimate_rows <- NULL
  if (!is.null(estimates)) {
    assert_epidist.epidist_estimates_data(estimates)
    estimate_rows <- .meta_estimate_rows(estimates)
  }
  meta_data <- bind_rows(individual_rows, estimate_rows)
  if (hasName(meta_data, "study")) {
    meta_data$study <- as.character(meta_data$study)
    meta_data$study[is.na(meta_data$study)] <- "individual"
  }
  meta_data <- new_epidist_meta_model(meta_data)
  assert_epidist(meta_data)
  return(meta_data)
}

#' Build the individual level rows of an `epidist_meta_model` object
#'
#' @param data Individual level data prepared by [.prepare_marginal_data()].
#'
#' @returns A tibble of individual level rows using the meta model slots.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_individual_rows <- function(data) {
  rows <- tibble(
    delay_lwr = as.integer(data$delay_lwr),
    n = data$n,
    obs_type = 1L,
    study_n = 0L,
    trunc_adjusted = 0L,
    cens_adjusted = 0L,
    relative_obs_time = as.numeric(data$relative_obs_time),
    pwindow = as.numeric(data$pwindow),
    swindow = as.numeric(data$swindow),
    delay_upr = as.numeric(data$delay_upr),
    report_se = 0,
    quantile_p = 0,
    growth_rate = 0
  )
  extra <- data[setdiff(names(data), names(rows))]
  return(bind_cols(rows, extra))
}

#' Build the summary estimate rows of an `epidist_meta_model` object
#'
#' @param estimates An `epidist_estimates_data` object.
#'
#' @returns A tibble of summary rows using the meta model slots.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_estimate_rows <- function(estimates) {
  rows <- tibble(
    delay_lwr = 0L,
    n = 1,
    obs_type = .meta_obs_type(estimates$type),
    study_n = as.integer(ifelse(is.na(estimates$n), 0L, estimates$n)),
    trunc_adjusted = as.integer(estimates$trunc_adjusted),
    cens_adjusted = as.integer(estimates$cens_adjusted),
    relative_obs_time = as.numeric(.estimates_grid_cutoff(estimates)),
    pwindow = as.numeric(estimates$pwindow),
    swindow = as.numeric(estimates$swindow),
    delay_upr = as.numeric(estimates$value),
    report_se = ifelse(is.na(estimates$se), 0, estimates$se),
    quantile_p = ifelse(is.na(estimates$p), 0, estimates$p),
    growth_rate = as.numeric(estimates$growth_rate)
  )
  consumed <- setdiff(.estimates_required_cols(), "study")
  extra <- estimates[setdiff(names(estimates), c(names(rows), consumed))]
  return(bind_cols(rows, extra))
}

#' Map summary types to their observation type codes
#'
#' @param type A character vector of summary types.
#'
#' @returns An integer vector of observation type codes.
#'
#' @keywords internal
.meta_obs_type <- function(type) {
  return(match(type, .estimates_types()) + 1L)
}

#' Class constructor for `epidist_meta_model` objects
#'
#' @param data A data.frame to convert
#'
#' @returns An object of class `epidist_meta_model`
#'
#' @family meta_model
#' @export
new_epidist_meta_model <- function(data) {
  class(data) <- c("epidist_meta_model", class(data))
  return(data)
}

#' Check if data has the `epidist_meta_model` class
#'
#' @param data A `data.frame` to check.
#'
#' @family meta_model
#' @export
is_epidist_meta_model <- function(data) {
  return(inherits(data, "epidist_meta_model"))
}

#' Assert validity of `epidist_meta_model` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments
#'
#' @method assert_epidist epidist_meta_model
#'
#' @family meta_model
#' @export
assert_epidist.epidist_meta_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = .meta_required_cols())
  assert_subset(data$obs_type, 1:4, .var.name = "obs_type")
  assert_integerish(data$delay_lwr)
  assert_numeric(data$n, lower = 0)
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_numeric(data$relative_obs_time, lower = 0)
  assert_numeric(data$delay_upr, lower = 0)
  assert_numeric(data$report_se, lower = 0)
  assert_numeric(data$quantile_p, lower = 0, upper = 1)
  assert_numeric(data$growth_rate, finite = TRUE)

  individual <- data[data$obs_type == 1L, , drop = FALSE]
  if (nrow(individual) > 0) {
    if (!all(
      abs(individual$delay_upr - (individual$delay_lwr + individual$swindow)) <
        1e-10
    )) {
      cli::cli_abort(
        "delay_upr must equal delay_lwr + swindow for individual level rows"
      )
    }
    if (!all(individual$relative_obs_time >= individual$delay_upr)) {
      cli::cli_abort(paste0(
        "relative_obs_time must be greater than or equal to delay_upr for ",
        "individual level rows"
      ))
    }
  }

  summaries <- data[data$obs_type != 1L, , drop = FALSE]
  if (nrow(summaries) > 0) {
    if (any(summaries$study_n < 2 & summaries$report_se <= 0)) {
      cli::cli_abort(paste0(
        "Summary rows need either a study sample size of at least 2 or a ",
        "reported standard error."
      ))
    }
    if (!all(is.finite(summaries$relative_obs_time))) {
      cli::cli_abort(
        "Summary rows need a finite grid cutoff in relative_obs_time"
      )
    }
    if (any(summaries$relative_obs_time < summaries$swindow)) {
      cli::cli_abort(
        "The grid cutoff for summary rows must be at least swindow"
      )
    }
    quantiles <- summaries[summaries$obs_type == 4L, , drop = FALSE]
    if (any(quantiles$quantile_p <= 0 | quantiles$quantile_p >= 1)) {
      cli::cli_abort(paste0(
        "Quantile rows need a probability strictly between 0 and 1 in ",
        "quantile_p"
      ))
    }
  }

  return(invisible(NULL))
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_family_model epidist_meta_model
#'
#' @family meta_model
#' @export
epidist_family_model.epidist_meta_model <- function(
  data,
  family,
  ...
) {
  custom_family <- brms::custom_family(
    paste0("meta_", family$family),
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
      paste0("vint", 1:4, "[n]"),
      paste0("vreal", 1:7, "[n]"),
      "primary_params"
    ),
    loop = TRUE,
    log_lik = epidist_gen_meta_log_lik(family),
    posterior_predict = epidist_gen_meta_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula for the
#' meta model
#'
#' @inheritParams epidist_formula_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_formula_model epidist_meta_model
#'
#' @family meta_model
#' @export
epidist_formula_model.epidist_meta_model <- function(
  data,
  formula,
  ...
) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula,
    delay_lwr |
      weights(n) +
        vint(obs_type, study_n, trunc_adjusted, cens_adjusted) +
        vreal(
          relative_obs_time,
          pwindow,
          swindow,
          delay_upr,
          report_se,
          quantile_p,
          growth_rate
        ) ~
      .
  )
  return(formula)
}

#' Transform data for the meta model
#'
#' Individual level rows are summarised by counting unique combinations of the
#' meta model columns and any variables in the model formula, exactly as
#' [epidist_transform_data_model.epidist_marginal_model()] does. Summary rows
#' are passed through unchanged because each one is a distinct reported
#' quantity.
#'
#' @param data The data to transform
#'
#' @param family The epidist family object specifying the distribution
#'
#' @param formula The model formula
#'
#' @param ... Additional arguments passed to methods
#'
#' @method epidist_transform_data_model epidist_meta_model
#' @family meta_model
#' @autoglobal
#' @export
epidist_transform_data_model.epidist_meta_model <- function(
  data,
  family,
  formula,
  ...
) {
  required_cols <- .meta_required_cols()
  extra_cols <- intersect("study", names(data))
  plain_data <- tibble::as_tibble(unclass(data))

  individual <- plain_data[plain_data$obs_type == 1L, , drop = FALSE]
  summaries <- plain_data[plain_data$obs_type != 1L, , drop = FALSE]

  if (nrow(individual) > 0) {
    individual <- .summarise_n_by_formula(
      individual,
      by = c(required_cols, extra_cols),
      formula = formula
    )
  }

  trans_data <- new_epidist_meta_model(bind_rows(individual, summaries))

  .inform_data_summarised(data, trans_data, c(required_cols, extra_cols))

  return(trans_data)
}

#' @method epidist_stancode epidist_meta_model
#' @importFrom brms stanvar
#' @family meta_model
#' @autoglobal
#' @export
epidist_stancode.epidist_meta_model <- function(
  data,
  family = epidist_family(data),
  formula = epidist_formula(data),
  ...
) {
  assert_epidist.epidist_meta_model(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("meta_model", "functions.stan"))
  )

  family_name <- gsub("meta_", "", family$name, fixed = TRUE)

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
    scode = primarycensored::pcd_load_stan_functions(
      c(
        "primarycensored_lpmf",
        "primarycensored_lcdf",
        "primarycensored_ode",
        "dist_lcdf",
        "primary_lpdf"
      ),
      dependencies = TRUE
    )
  )

  stanvars_all <- stanvars_version +
    stanvars_functions +
    pcd_stanvars_functions +
    stanvars_parameters

  return(stanvars_all)
}

.meta_required_cols <- function() {
  return(c(
    "delay_lwr",
    "n",
    "obs_type",
    "study_n",
    "trunc_adjusted",
    "cens_adjusted",
    "relative_obs_time",
    "pwindow",
    "swindow",
    "delay_upr",
    "report_se",
    "quantile_p",
    "growth_rate"
  ))
}
