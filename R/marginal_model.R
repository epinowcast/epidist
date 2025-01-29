#' Prepare marginal model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @param ... Additional arguments passed to methods.
#' @family marginal_model
#' @export
as_epidist_marginal_model <- function(data, ...) {
  UseMethod("as_epidist_marginal_model")
}

#' The marginal model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object
#' @param obs_time_threshold Ratio used to determine threshold for setting
#'   relative observation times to Inf. Observation times greater than
#'   `obs_time_threshold` times the maximum delay will be set to Inf to improve
#'   model efficiency by reducing the number of unique observation times.
#'   Default is 2.
#' @param weight A column name to use for weighting the data in the
#'   likelihood. Default is NULL. Internally this is used to define the 'n'
#'   column of the returned object. See details.
#'   column of the returned object. See details.
#' @param ... Not used in this method.
#' @details To ensure efficient computation, the model automatically
#' identifies groups of individuals in the data that contribute identically
#' to the likelihood, and evaluates the likelihood at the group level. Groups
#' are defined by unique combinations of: {ptime_upr, stime_upr, stime_lwr,
#' relative_obs_time, pwindow, swindow, and other_vars}, where the first six
#' fields are defined by `as_epidist_linelist_data()`,
#' and where `other_vars` includes other variables used in the model forumla,
#' (e.g. age, sex, or location).
#'
#' The `weight` option can be used for convenience when working with data that
#' contain too many individuals to represent easily in linelist format. For
#' example, `prepped` and `prepped_weighted` would be
#' interpreted identically by epidist():
#'
#' ```
#' ## Prep as a linelist
#' prepped <- linelist |>
#'   as_epidist_linelist_data() |>
#'   as_epidist_marginal_model(linelist)
#' ```
#'
#' or
#'
#' ```
#' ## Aggregate by relevant variables
#' prepped_weighted <- linelist |>
#'  as_epidist_linelist_data() |>
#'  # summarise by unique combinations of variables
#'   group_by(ptime_upr, stime_upr, stime_lwr,
#'            relative_obs_time, pwindow, swindow,
#'            other_vars) |>
#' summarise(n = n())
#' # prep with weight option
#' as_epidist_marginal_model(linelist, weight = "n")
#' ```
#' @method as_epidist_marginal_model epidist_linelist_data
#' @family marginal_model
#' @autoglobal
#' @export
as_epidist_marginal_model.epidist_linelist_data <- function(
    data, obs_time_threshold = 2, weight = NULL, ...) {
  assert_epidist(data)

  data <- data |>
    mutate(
      pwindow = .data$ptime_upr - .data$ptime_lwr,
      swindow = .data$stime_upr - .data$stime_lwr,
      relative_obs_time = .data$obs_time - .data$ptime_lwr,
      orig_relative_obs_time = .data$obs_time - .data$ptime_lwr,
      delay_lwr = .data$stime_lwr - .data$ptime_lwr,
      delay_upr = .data$stime_upr - .data$ptime_lwr
    )

  if (!is.null(weight)) {
    assert_names(names(data), must.include = weight)
    data <- data |>
      mutate(n = .data[[weight]])
  } else {
    data <- data |>
      mutate(n = 1)
  }

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
  assert_names(names(data), must.include = c(
    "pwindow", "swindow", "delay_lwr", "delay_upr", "n",
    "relative_obs_time"
  ))
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
}

#' Check if data has the `epidist_marginal_model` class
#'
#' @param data A `data.frame` containing line list data
#' @family marginal_model
#' @export
is_epidist_marginal_model <- function(data) {
  inherits(data, "epidist_marginal_model")
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#' @param ... Additional arguments passed to method.
#' @method epidist_family_model epidist_marginal_model
#' @family marginal_model
#' @export
epidist_family_model.epidist_marginal_model <- function(
    data, family, ...) {
  custom_family <- brms::custom_family(
    paste0("marginal_", family$family),
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub"))),
    type = "int",
    vars = c(
      "vreal1[n]", "vreal2[n]", "vreal3[n]", "vreal4[n]", "primary_params"
    ),
    loop = TRUE,
    log_lik = epidist_gen_log_lik(family),
    posterior_predict = epidist_gen_posterior_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula
#'
#' @inheritParams epidist_formula_model
#' @param ... Additional arguments passed to method.
#' @method epidist_formula_model epidist_marginal_model
#' @family marginal_model
#' @export
epidist_formula_model.epidist_marginal_model <- function(
    data, formula, ...) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula, delay_lwr | weights(n) +
      vreal(relative_obs_time, pwindow, swindow, delay_upr) ~ .
  )
  return(formula)
}

#' @method epidist_transform_data_model epidist_marginal_model
#' @family marginal_model
#' @importFrom purrr map_chr
#' @export
epidist_transform_data_model.epidist_marginal_model <- function(
    data, family, formula, ...) {
  required_cols <- c(
    "delay_lwr", "delay_upr", "relative_obs_time", "pwindow", "swindow"
  )
  n_rows_before <- nrow(data)

  trans_data <- data |>
    .summarise_n_by_formula(by = required_cols, formula = formula) |>
    new_epidist_marginal_model()
  n_rows_after <- nrow(trans_data)
  if (n_rows_before > n_rows_after) {
    cli::cli_inform(c(
      "i" = "Data summarised by unique combinations of:" # nolint
    ))

    formula_vars <- setdiff(names(trans_data), c(required_cols, "n"))
    if (length(formula_vars) > 0) {
      cli::cli_inform(c(
        "*" = "Formula variables: {.code {formula_vars}}"
      ))
    }

    cli::cli_inform(paste0(
      "* Model variables: delay bounds, observation time, ",
      "and primary censoring window"
    ))

    cli::cli_inform(c(
      "!" = paste("Reduced from", n_rows_before, "to", n_rows_after, "rows."),
      "i" = "This should improve model efficiency with no loss of information." # nolint
    ))
  }

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
    formula = epidist_formula(data), ...) {
  assert_epidist(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("marginal_model", "functions.stan"))
  )

  family_name <- gsub("marginal_", "", family$name, fixed = TRUE)

  stanvars_functions[[1]]$scode <- gsub(
    "family", family_name, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  if (family_name == "lognormal") {
    dist_id <- 1
  } else if (family_name == "gamma") {
    dist_id <- 2
  } else if (family_name == "weibell") {
    dist_id <- 3
  } else {
    cli_abort(c(
      "!" = "epidist does not currently support this family for the marginal model" # nolint
    ))
  }

  # Replace the dist_id passed to primarycensored
  stanvars_functions[[1]]$scode <- gsub(
    "dist_id", dist_id, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_A",
    toString(paste0("real ", family$dpars)),
    stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "dpars_B", family$param, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "primary_id", "1", stanvars_functions[[1]]$scode,
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

  stanvars_all <- stanvars_version + stanvars_functions +
    pcd_stanvars_functions + stanvars_parameters

  return(stanvars_all)
}
