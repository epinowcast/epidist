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
#'  [as_epidist_marginal_model()], [as_epidist_latent_model()] or
#'  [as_epidist_naive_model()].
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
           {.fn as_epidist_marginal_model} or {.fn as_epidist_naive_model}
           first."
    )
  ))
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
