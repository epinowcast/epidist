#' Build `newdata` for prediction from an `epidist` data object
#'
#' Prediction with `brms` and `tidybayes` needs a `newdata` argument holding
#' every variable the model uses. As well as the variables in your formula
#' that means the response and the observation process variables that
#' `epidist` adds. Their names depend on the model, so building `newdata` by
#' hand means knowing how each model is specified. This function builds it for
#' you. It expands the variables you supply into a grid, in the same way as
#' [tidyr::expand()], and adds the rest with values you set through named
#' arguments.
#'
#' The defaults give the delay distribution with no censoring and no
#' truncation. Set `pwindow` and `swindow` to ask for censoring, and
#' `relative_obs_time` to ask for truncation. See the method for each model
#' for the arguments it takes.
#'
#' The result is a plain [tibble::tibble()]. Pass it to
#' [brms::posterior_epred()], [brms::posterior_predict()],
#' [add_delay_parameter_draws()] or [delay_parameter_draws()], or to the
#' `tidybayes` functions `add_epred_draws()` and `add_predicted_draws()`.
#'
#' @param data An `epidist` data object, such as one returned by
#'  [as_epidist_marginal_model()], [as_epidist_latent_model()],
#'  [as_epidist_naive_model()] or [as_epidist_meta_model()].
#'
#' @param ... Variables to expand into a grid, passed to [tidyr::expand()].
#'  Supply the variables used in the model formula, such as `sex`. Each
#'  combination of their unique values becomes a row. Supply no variables to
#'  get a single row, which is what an intercept only model needs. A variable
#'  expanded here keeps its expanded values, so naming it as an argument of
#'  the method as well is an error.
#'
#' @family newdata
#' @returns A [tibble::tibble()] of `newdata` ready to predict from.
#'
#' @export
#' @examples
#' prep_obs <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_marginal_model()
#'
#' # An intercept only model
#' epidist_newdata(prep_obs)
#'
#' # A row for each sex
#' epidist_newdata(prep_obs, sex)
epidist_newdata <- function(data, ...) {
  UseMethod("epidist_newdata")
}

#' Default method for building `newdata`
#'
#' @inheritParams epidist_newdata
#'
#' @method epidist_newdata default
#' @family newdata
#' @returns This method errors. It is called when `data` is not an `epidist`
#'  model data object.
#'
#' @export
epidist_newdata.default <- function(data, ...) {
  return(cli::cli_abort(
    c(
      "No {.fn epidist_newdata} method is available for objects of class
       {.cls {class(data)}}.",
      i = "Convert your data with {.fn as_epidist_latent_model},
           {.fn as_epidist_marginal_model}, {.fn as_epidist_naive_model} or
           {.fn as_epidist_meta_model} first."
    )
  ))
}

#' Build `newdata` for the meta model
#'
#' The meta model holds individual level rows and summary rows in one data
#' frame, telling them apart by the `obs_type` slot. Prediction is for the
#' delay distribution itself, so this method builds an individual level row.
#' It takes the same arguments as [epidist_newdata.epidist_marginal_model()],
#' and fills the slots a summary row would use with the values an individual
#' row carries. The result is what [delay_parameter_draws()] and
#' [add_summaries()] need, so there is no need to copy a summary row out of
#' the model data and overwrite its covariates.
#'
#' A model with a study level term, such as `mu ~ 1 + (1 | study)`, needs
#' either a `study` column in `newdata` or `re_formula = NA` when predicting.
#' Expand `study` here to predict for each study, or leave it out and pass
#' `re_formula = NA` for the population level delay. The primary event
#' distribution of the individual level rows is a parameter of the model
#' rather than a column, so it needs nothing here.
#'
#' @inheritParams epidist_newdata.epidist_marginal_model
#'
#' @method epidist_newdata epidist_meta_model
#' @family meta_model
#' @family newdata
#' @returns A [tibble::tibble()] of `newdata` ready to predict from.
#'
#' @export
#' @examples
#' estimates <- as_epidist_estimates_data(
#'   data.frame(
#'     study = c("A", "A", "B"),
#'     type = c("mean", "sd", "mean"),
#'     value = c(7.5, 3.6, 6.4),
#'     n = c(120, 120, 80),
#'     relative_obs_time = c(20, 20, Inf),
#'     trunc_adjusted = c(FALSE, FALSE, TRUE),
#'     cens_adjusted = c(0, 0, 1)
#'   )
#' )
#' meta <- as_epidist_meta_model(estimates = estimates)
#'
#' # The population level delay, with no censoring and no truncation
#' epidist_newdata(meta)
#'
#' # A row for each study, with daily censoring
#' epidist_newdata(meta, study, pwindow = 1, swindow = 1)
epidist_newdata.epidist_meta_model <- function(
  data,
  ...,
  pwindow = 0,
  swindow = 0,
  relative_obs_time = Inf,
  delay_min = 0
) {
  assert_numeric(pwindow, lower = 0, any.missing = FALSE)
  assert_numeric(swindow, lower = 0, any.missing = FALSE)
  assert_numeric(relative_obs_time, lower = 0, any.missing = FALSE)
  assert_numeric(delay_min, lower = 0, any.missing = FALSE)
  newdata <- .build_newdata(
    data,
    ...,
    .cols = list(
      delay_lwr = 0,
      obs_type = 1L,
      study_n = 0L,
      trunc_adjusted = 0L,
      trunc_design = 0L,
      cens_adjusted = 0L,
      group_start = 1L,
      group_len = 0L,
      chol_start = 1L,
      n_quad = .meta_n_quad(),
      relative_obs_time = relative_obs_time,
      pwindow = pwindow,
      swindow = swindow,
      delay_upr = 0,
      delay_min = delay_min,
      report_se = 0,
      quantile_p = 0,
      growth_rate = 0
    ),
    .supplied = intersect(names(match.call()), names(formals()))
  )
  return(newdata)
}

#' Expand variables into a grid and add the variables a model needs
#'
#' Used by the [epidist_newdata()] methods. Any `epidist` class is dropped
#' before expanding so that the class methods documented in [epidist_data] do
#' not warn about an object that was never meant to stay in its class.
#'
#' @param data An `epidist` data object.
#'
#' @param ... Variables to expand into a grid, passed to [tidyr::expand()].
#'
#' @param .cols A named list of the variables to add, each of which is crossed
#'  with the grid.
#'
#' @param .supplied Names of the method's own arguments that the user gave,
#'  usually `intersect(names(match.call()), names(formals()))`. A name that was
#'  both expanded and supplied is an error. Names passed through `...` are not
#'  included, so setting a column with the `tidyr::expand()` syntax, such as
#'  `pwindow = 1:2`, still works.
#'
#' @returns A [tibble::tibble()] of `newdata`.
#'
#' @keywords internal
#' @importFrom tidyr expand expand_grid
.build_newdata <- function(data, ..., .cols, .supplied = character()) {
  data <- .drop_epidist_class(data)
  expanded <- if (...length() > 0) tidyr::expand(data, ...) else NULL
  clash <- intersect(intersect(names(.cols), .supplied), names(expanded))
  if (length(clash) > 0) {
    cli::cli_abort(
      c(
        "{.var {clash}} {?was/were} both expanded and given as an argument.",
        i = "Drop {cli::qty(clash)}{?it/them} from one or the other."
      )
    )
  }
  .cols <- .cols[setdiff(names(.cols), names(expanded))]
  cols <- if (is.null(expanded)) .cols else c(list(expanded), .cols)
  newdata <- do.call(tidyr::expand_grid, cols)
  return(newdata)
}
