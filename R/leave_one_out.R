#' Refit a meta model leaving each study out in turn
#'
#' @description
#' A pooled estimate should not rest on a single study.
#' This refits a meta model once per study, each time with that study's rows
#' removed, and compares the natural scale mean and standard deviation of the
#' delay from each refit with the full fit.
#' Each refit is exact rather than an importance sampling approximation.
#' [loo::loo()] works per observation, which for the meta model is a group of
#' summaries rather than a study, and the number of studies is usually small
#' enough that refitting costs little.
#'
#' @details
#' The study of each row is the `study` column of the model data.
#' Individual level rows, which [as_epidist_meta_model()] labels
#' `"individual"`, are held out together as one study.
#' `brms` keeps only the variables the formula uses in a fit, so a model whose
#' formula does not mention `study`, such as `mu ~ 1`, has no `study` column
#' in `fit$data`.
#' Pass the `epidist_meta_model` object the model was fitted to as `data` in
#' that case.
#' Removing a study then goes through the [epidist_data] methods, so the
#' object is checked, and through [epidist_transform_data()], so the rows are
#' summarised as they were for the full fit.
#'
#' Each refit reuses the compiled model through [brms::update.brmsfit()] with
#' the data of the held out study removed.
#' The formula, family, priors and sampler settings of the full fit are kept
#' unless overridden through `...`.
#' The delay summaries come from [delay_summary_draws()] at `newdata`.
#' The default is the population level delay with no censoring and no
#' truncation, built with [epidist_newdata()], with any study level term
#' switched off by `re_formula = NA`.
#' Every refit must be able to predict `newdata`, so it must not name the
#' held out study, and every level of a factor it uses must remain in the
#' data once each study is removed.
#'
#' For each summary the posterior median and the central `width` interval of
#' the held out fit are reported next to those of the full fit.
#' `shift` is the difference between the two medians divided by the posterior
#' standard deviation of the full fit.
#' `influential` is `TRUE` when the held out median lies outside the full
#' fit's interval, that is when removing the study moves the estimate by more
#' than the full fit's own uncertainty.
#' With few studies every refit loses a large share of the evidence, so the
#' intervals of the refits are wider than the full fit's.
#' The comparison is of the point estimate against the full fit's interval,
#' so it is not affected by that.
#'
#' @param fit A meta model fitted with [epidist()] to an `epidist_meta_model`
#'  object.
#'
#' @param data The `epidist_meta_model` object `fit` was fitted to.
#'  Only needed when `fit$data` has no `study` column, which happens when the
#'  formula does not use `study`.
#'  If `NULL`, the default, the model data stored in `fit` is used.
#'
#' @param newdata A `data.frame` of data to predict the delay for, passed to
#'  [delay_summary_draws()].
#'  If `NULL`, the default, a single row giving the population level delay
#'  with no censoring and no truncation is built with [epidist_newdata()].
#'
#' @param re_formula Passed to [brms::prepare_predictions()] through
#'  [delay_summary_draws()].
#'  The default `NA` switches off any study level term, so the summaries are
#'  of the population level delay.
#'
#' @param width The width of the central posterior interval reported for each
#'  summary, and used to flag influential studies.
#'  Defaults to `0.95`.
#'
#' @param keep_fits If `TRUE`, the refits are returned in the `fits` attribute
#'  of the result as a list named by the held out study.
#'  Defaults to `FALSE`.
#'
#' @param ... Additional arguments passed to [brms::update.brmsfit()] and so
#'  to [brms::brm()], such as `cores`, `chains`, `iter`, `refresh` and
#'  `silent`.
#'
#' @family meta_model
#' @returns A `tibble` with one row per held out study, row of `newdata` and
#'  summary.
#'  The columns are `study`, `.row`, any predictors of the model present in
#'  `newdata` other than `study` itself, `summary` (`"mean"` or `"sd"`),
#'  `estimate`, `lower` and `upper` from the held out fit, `full_estimate`,
#'  `full_lower` and `full_upper` from the full fit, `shift` and
#'  `influential`.
#'  `study` always names the held out study, even when `newdata` also has a
#'  `study` column, such as one built with `epidist_newdata(meta, study)`.
#'  The `width` attribute records the interval width.
#'
#' @seealso [delay_summary_draws()] for the summaries the comparison uses and
#'  [epidist_newdata()] to build `newdata`.
#'
#' @export
#' @examples
#' \donttest{
#' estimates <- as_epidist_estimates_data(
#'   data.frame(
#'     study = c("A", "A", "B", "B", "C", "C"),
#'     type = c("mean", "sd", "mean", "sd", "mean", "sd"),
#'     value = c(7.5, 3.6, 6.4, 3.1, 8.2, 4.0),
#'     n = c(120, 120, 80, 80, 150, 150),
#'     relative_obs_time = c(20, 20, 25, 25, Inf, Inf),
#'     trunc_adjusted = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
#'     cens_adjusted = c(0, 0, 0, 0, 1, 1)
#'   )
#' )
#' meta <- as_epidist_meta_model(estimates = estimates)
#' fit <- epidist(
#'   meta,
#'   chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0)
#' )
#'
#' # The formula does not use study, so pass the model data
#' epidist_leave_one_out(fit, data = meta, refresh = 0)
#' }
epidist_leave_one_out <- function(
  fit,
  data = NULL,
  newdata = NULL,
  re_formula = NA,
  width = 0.95,
  keep_fits = FALSE,
  ...
) {
  .assert_meta_fit(fit)
  assert_number(width, lower = 0, upper = 1)
  assert_flag(keep_fits)
  model_data <- .leave_one_out_data(fit, data)
  studies <- .leave_one_out_studies(model_data)
  if (is.null(newdata)) {
    newdata <- .leave_one_out_newdata(fit)
  }
  assert_data_frame(newdata, min.rows = 1)
  full <- .leave_one_out_summaries(fit, newdata, re_formula, width)
  refits <- lapply(studies, function(study) {
    held_out <- .drop_study(model_data, study, fit)
    return(.refit(fit, held_out, ...))
  })
  names(refits) <- studies
  held <- lapply(refits, .leave_one_out_summaries, newdata, re_formula, width)
  held <- bind_rows(held, .id = "study")
  out <- .leave_one_out_compare(held, full)
  out <- .leave_one_out_bind_predictors(out, newdata, fit$formula)
  attr(out, "width") <- width
  if (keep_fits) {
    attr(out, "fits") <- refits
  }
  return(out)
}

#' Check that a fit is a meta model fitted with `epidist()`
#'
#' @inheritParams epidist_leave_one_out
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
.assert_meta_fit <- function(fit) {
  if (!inherits(fit, "epidist_fit")) {
    cli_abort("{.arg fit} must be a model fitted with {.fn epidist}.")
  }
  if (!startsWith(.fit_family_name(fit), "meta_")) {
    cli_abort(c(
      "{.arg fit} must be a fit of the meta model.",
      i = "Build the data with {.fn as_epidist_meta_model}."
    ))
  }
  return(invisible(NULL))
}

#' The name of the `brms` family a fit was made with
#'
#' The families `epidist` builds are custom families named after the model
#' and the delay distribution, such as `meta_lognormal`.
#'
#' @inheritParams epidist_leave_one_out
#'
#' @returns A string.
#'
#' @keywords internal
.fit_family_name <- function(fit) {
  name <- fit$family$name
  if (is.null(name)) {
    name <- fit$family$family
  }
  return(as.character(name))
}

#' The model data used to hold each study out of a meta model fit
#'
#' @inheritParams epidist_leave_one_out
#'
#' @returns The `epidist_meta_model` object given as `data`, or the plain
#'  `data.frame` of model data stored in `fit`.
#'
#' @keywords internal
.leave_one_out_data <- function(fit, data = NULL) {
  if (is.null(data)) {
    data <- fit$data
    if (!hasName(data, "study")) {
      cli_abort(c(
        "The model data of {.arg fit} has no {.var study} column.",
        i = paste0(
          "{.pkg brms} keeps only the variables the formula uses, so pass ",
          "the {.cls epidist_meta_model} object the model was fitted to as ",
          "{.arg data}."
        )
      ))
    }
    return(data)
  }
  if (!is_epidist_meta_model(data)) {
    cli_abort(c(
      "{.arg data} must be an {.cls epidist_meta_model} object.",
      i = "Build it with {.fn as_epidist_meta_model}."
    ))
  }
  return(data)
}

#' The studies of a meta model, in order of first appearance
#'
#' @param data An `epidist_meta_model` object or the model data of a meta
#'  model fit, with a `study` column.
#'
#' @returns A character vector of study labels.
#'
#' @keywords internal
.leave_one_out_studies <- function(data) {
  if (!hasName(data, "study")) {
    cli_abort(c(
      "{.arg data} has no {.var study} column, so there is nothing to hold
       out.",
      i = paste0(
        "A meta model of individual level rows alone is a single study. ",
        "Add summary estimates, or a {.var study} column, to compare studies."
      )
    ))
  }
  studies <- unique(as.character(data$study))
  if (length(studies) < 2) {
    cli_abort(paste0(
      "The model data holds only the study {.val {studies}}, so there is ",
      "nothing to hold it out against."
    ))
  }
  return(studies)
}

#' The default `newdata` for comparing leave one out refits
#'
#' A single row giving the population level delay with no censoring and no
#' truncation. [epidist_newdata()] only reads its `data` argument to expand
#' variables, so a copy of the model data carrying the class is enough.
#'
#' @inheritParams epidist_leave_one_out
#'
#' @returns A [tibble::tibble()] of one row.
#'
#' @keywords internal
.leave_one_out_newdata <- function(fit) {
  primary <- fit$family$primary
  if (is.null(primary)) {
    primary <- "uniform"
  }
  model_data <- new_epidist_meta_model(
    tibble::as_tibble(fit$data),
    primary = primary
  )
  return(epidist_newdata(model_data))
}

#' Remove one study from the model data of a meta model
#'
#' An `epidist_meta_model` object is subset through the [epidist_data]
#' methods, which check the result, and then transformed with
#' [epidist_transform_data()] so that its rows are summarised as they were
#' for the full fit. The plain model data stored in a fit has already been
#' transformed, so it is subset directly.
#'
#' @inheritParams .leave_one_out_studies
#'
#' @param study The label of the study to remove.
#'
#' @param fit The fit whose family and formula the transform uses.
#'
#' @returns The model data without the rows of `study`.
#'
#' @keywords internal
.drop_study <- function(data, study, fit) {
  held_out <- data[data$study != study, , drop = FALSE]
  if (is_epidist_meta_model(held_out)) {
    held_out <- suppressMessages(
      epidist_transform_data(held_out, fit$family, fit$formula)
    )
  }
  return(held_out)
}

#' Refit a model to new data, reusing the compiled model
#'
#' @inheritParams epidist_leave_one_out
#'
#' @param newdata The data to refit to.
#'
#' @returns A fit with the classes of `fit`.
#'
#' @keywords internal
.refit <- function(fit, newdata, ...) {
  refit <- stats::update(fit, newdata = newdata, ...)
  class(refit) <- class(fit)
  return(refit)
}

#' Posterior summaries of the delay mean and standard deviation of a fit
#'
#' @inheritParams epidist_leave_one_out
#'
#' @param newdata A `data.frame` of data to predict the delay for, passed to
#'  [delay_summary_draws()].
#'  Always an already resolved `data.frame`, never `NULL`.
#'
#' @returns A `tibble` with one row per row of `newdata` and summary, with
#'  columns `.row`, `summary`, `estimate` (the posterior median), `lower`,
#'  `upper` and `posterior_sd`.
#'
#' @keywords internal
#' @autoglobal
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
.leave_one_out_summaries <- function(fit, newdata, re_formula = NA, width) {
  draws <- delay_summary_draws(fit, newdata = newdata, re_formula = re_formula)
  draws <- dplyr::ungroup(draws)
  probs <- c((1 - width) / 2, 1 - (1 - width) / 2)
  long <- pivot_longer(
    draws[c(".row", "mean", "sd")],
    c("mean", "sd"),
    names_to = "summary",
    values_to = "value"
  )
  out <- summarise(
    long,
    estimate = stats::median(.data$value),
    lower = stats::quantile(.data$value, probs[1], names = FALSE),
    upper = stats::quantile(.data$value, probs[2], names = FALSE),
    posterior_sd = stats::sd(.data$value),
    .by = c(".row", "summary")
  )
  return(out)
}

#' Compare the summaries of leave one out refits with the full fit
#'
#' @param held A `tibble` of summaries of the refits, as returned by
#'  [.leave_one_out_summaries()], with a `study` column naming the held out
#'  study.
#'
#' @param full The summaries of the full fit, as returned by
#'  [.leave_one_out_summaries()].
#'
#' @returns `held` with the full fit values added as `full_estimate`,
#'  `full_lower` and `full_upper`, the standardised `shift` and the
#'  `influential` flag.
#'
#' @keywords internal
.leave_one_out_compare <- function(held, full) {
  full <- full[c(
    ".row", "summary", "estimate", "lower", "upper", "posterior_sd"
  )]
  names(full) <- c(
    ".row", "summary", "full_estimate", "full_lower", "full_upper", "full_sd"
  )
  out <- dplyr::left_join(held, full, by = c(".row", "summary"))
  out$shift <- (out$estimate - out$full_estimate) / out$full_sd
  out$influential <- out$estimate < out$full_lower |
    out$estimate > out$full_upper
  out <- out[c(
    "study", ".row", "summary", "estimate", "lower", "upper",
    "full_estimate", "full_lower", "full_upper", "shift", "influential"
  )]
  return(tibble::as_tibble(out))
}

#' Attach the model's predictors from `newdata` to a comparison
#'
#' `study` in `out` already names the held out study, so a `study` predictor
#' in `newdata`, such as one built with `epidist_newdata(meta, study)`, is
#' excluded to avoid a second column of the same name.
#'
#' @param out The comparison, as returned by [.leave_one_out_compare()], with
#'  `study` and `.row` columns.
#'
#' @param newdata The `newdata` the comparison was built from.
#'
#' @param formula The `brms` formula of the fit, used to find its predictors.
#'
#' @returns `out` with any predictors of `formula` present in `newdata`,
#'  other than `study`, added after `.row`.
#'
#' @keywords internal
.leave_one_out_bind_predictors <- function(out, newdata, formula) {
  predictors <- intersect(.extract_dpar_terms(formula), names(newdata))
  predictors <- setdiff(predictors, "study")
  newdata <- tibble::as_tibble(newdata)
  return(bind_cols(
    out[c("study", ".row")],
    newdata[out$.row, predictors, drop = FALSE],
    out[setdiff(names(out), c("study", ".row"))]
  ))
}
