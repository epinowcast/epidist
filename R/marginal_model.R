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

  # Here we do the processing to turn an epidist_linelist_data into an aggregate
  # dataset. In the future this would be refactored into a function which
  # converts from linelist data to aggregate data, and a function which goes
  # from aggregate data into the marginal model class
  data <- data |>
    mutate(
      pwindow = ifelse(
        .data$stime_lwr < .data$ptime_upr,
        .data$stime_upr - .data$ptime_lwr,
        .data$ptime_upr - .data$ptime_lwr
      ),
      delay = .data$stime_lwr - .data$ptime_lwr
    ) |>
    dplyr::group_by(delay) |>
    dplyr::summarise(
      count = dplyr::n(),
      pwindow = ifelse(all(pwindow == pwindow[1]), pwindow[1], NA)
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
  assert_names(names(data), must.include = "delay")
  assert_numeric(data$delay, lower = 0)
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
    "primarycensored_wrapper",
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(NA, as.numeric(lapply(family$other_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(family$other_bounds, "[[", "ub"))),
    type = "int",
    loop = TRUE,
    vars = "vreal1[n]"
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
    formula, delay | weights(count) + vreal(pwindow) ~ .
  )
  return(formula)
}

#' @method epidist_stancode epidist_marginal_model
#' @importFrom brms stanvar
#' @family marginal_model
#' @autoglobal
#' @export
epidist_stancode.epidist_marginal_model <- function(data, ...) {
  assert_epidist(data)

  stanvars_version <- .version_stanvar()

  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("marginal_model", "functions.stan"))
  )

  pcd_stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = primarycensored::pcd_load_stan_functions()
  )

  stanvars_all <- stanvars_version + stanvars_functions + pcd_stanvars_functions

  return(stanvars_all)
}
