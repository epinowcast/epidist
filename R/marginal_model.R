#' Prepare marginal model to pass through to `brms`
#'
#' @param data A `data.frame` containing line list data
#' @family marginal_model
#' @export
as_epidist_marginal_model <- function(data) {
  UseMethod("as_epidist_marginal_model")
}

#' The marginal model method for `epidist_linelist_data` objects
#'
#' @param data An `epidist_linelist_data` object
#' @method as_epidist_marginal_model epidist_linelist_data
#' @family marginal_model
#' @autoglobal
#' @export
as_epidist_marginal_model.epidist_linelist_data <- function(data) {
  assert_epidist(data)

  data <- data |>
    mutate(
      pwindow = .data$ptime_upr - .data$ptime_lwr,
      swindow = .data$stime_upr - .data$stime_lwr,
      relative_obs_time = .data$obs_time - .data$ptime_lwr,
      delay_lwr = .data$stime_lwr - .data$ptime_lwr,
      delay_upr = .data$stime_upr - .data$ptime_lwr,
      n = 1
    )

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
  assert_numeric(data$delay_lwr)
  assert_numeric(data$delay_upr)
  assert_numeric(data$relative_obs_time)
  assert_true(
    all(abs(data$delay_upr - (data$delay_lwr + data$swindow)) < 1e-10),
    "delay_upr must equal delay_lwr + swindow"
  )
  assert_true(
    all(data$relative_obs_time >= data$delay_upr),
    "relative_obs_time must be greater than or equal to delay_upr"
  )
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
    type = family$type,
    vars = c(
      "vreal1[n]", "vreal2[n]", "vreal3[n]", "vreal4[n]"
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
      vreal(delay_upr, relative_obs_time, pwindow, swindow) ~ .
  )
  return(formula)
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

  # Can probably be extended to non-analytic solution families but for now
  if (family_name == "lognormal") {
    dist_id <- 1
  } else if (family_name == "gamma") {
    dist_id <- 2
  } else if (family_name == "weibell") {
    dist_id <- 3
  } else {
    cli_abort(c(
      "!" = "No analytic solution available in primarycensored for this family"
    ))
  }

  # Replace the dist_id passed to primarycensored
  stanvars_functions[[1]]$scode <- gsub(
    "dist_id", dist_id, stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

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

  stanvars_functions[[1]]$scode <- gsub(
    "primary_id", "1", stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  stanvars_functions[[1]]$scode <- gsub(
    "primary_params", "", stanvars_functions[[1]]$scode,
    fixed = TRUE
  )

  pcd_stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = primarycensored::pcd_load_stan_functions()
  )

  stanvars_all <- stanvars_version + stanvars_functions + pcd_stanvars_functions

  return(stanvars_all)
}
