#' Create an `epidist_estimates_data` object
#'
#' Creates an `epidist_estimates_data` object from published summary estimates
#' of a delay distribution.
#' Each row is a single reported summary component (a mean, a standard
#' deviation, or a quantile) together with the metadata needed to work out what
#' the study that reported it was actually estimating.
#' These objects are the summary data input to [as_epidist_meta_model()].
#' See the specific methods for details on supported input formats and usage
#' examples.
#'
#' The meta model these objects feed is experimental.
#' Its interface may still change in future releases.
#'
#' @param data The data to convert
#'
#' @param ... Additional arguments passed to methods
#'
#' @family estimates_data
#' @export
as_epidist_estimates_data <- function(data, ...) {
  UseMethod("as_epidist_estimates_data")
}

#' Create an `epidist_estimates_data` object from a data frame
#'
#' This method takes a `data.frame` of published summary estimates and creates
#' an `epidist_estimates_data` object.
#' Column names may either match the names used below or be supplied via the
#' corresponding argument.
#'
#' # What we need from each study
#'
#' Published delay estimates are almost never estimates of the true continuous
#' delay distribution.
#' To use them we forward model what the study's estimation procedure would
#' converge to, which means we need to know how the study handled the biases
#' we support, along with the data process it saw.
#' `vignette("model")` derives each estimand, and `vignette("meta")` works
#' through assembling this metadata for simulated and real studies.
#' For each study we need:
#'
#' * **How it adjusted for censoring** (`cens_adjusted`). A taxonomy of the
#'   common ways interval censored delays are summarised, deliberately limited
#'   to a few widely used approaches:
#'   * `0`: no adjustment. The study took integer date differences (for example
#'     date of onset subtracted from date of report) and summarised them
#'     directly. This is the most common case in the literature.
#'   * `1`: fully adjusted. The study used a method targeting the underlying
#'     continuous distribution, such as a double interval censored likelihood.
#'   * `2`: uniform single interval approximation. The study adjusted the
#'     secondary interval only, assuming a uniform delay within it, and left
#'     the primary interval uncorrected.
#'   * `3`: midpoint imputation. The study assigned each delay to the centre of
#'     the interval it was observed in, which shifts every reported delay up by
#'     half a secondary window without changing its spread.
#'   * `4`: midpoint imputation with a uniform interval. The study placed the
#'     primary event at the midpoint of its window and integrated the secondary
#'     interval. Common where the primary event has a wide exposure window and
#'     the secondary date is recorded precisely.
#'
#'   Use code `3` for a study that midpointed the secondary interval and left
#'   the primary alone. Anything more exotic must be approximated by whichever
#'   code is closest, and if you cannot tell which a study used, state the
#'   assumption you are making.
#' * **Whether it adjusted for right truncation** (`trunc_adjusted`) and, if it
#'   did not, **the observation time** (`relative_obs_time`) and **how
#'   collection stopped** (`trunc_design`). For a cohort the observation time is
#'   the truncation point on the delay scale. For an accrual design, where
#'   collection stopped at a calendar date, it is the length of the collection
#'   window, which is usually easier to read off a paper. Real time estimates
#'   are right truncated unless the study corrected for it.
#' * **The censoring windows** (`pwindow`, `swindow`). The width, in the same
#'   time units as the delay, of the interval each event was observed in. Daily
#'   reporting gives windows of 1, weekly reporting gives 7.
#' * **The sample size** (`n`), the number of delays the summary was computed
#'   from. This sets the sampling uncertainty on the reported value. A reported
#'   standard error (`se`) may be given instead, and takes precedence when
#'   supplied.
#' * **The minimum delay it counted** (`delay_min`), where the study dropped
#'   delays below some point. Its summaries then describe a left truncated
#'   delay distribution. Defaults to 0, meaning the study counted every delay.
#'
#' Systematic reviews rarely record this metadata, so you must supply your own
#' assumption and say so alongside any results. Where it is missing entirely, a
#' covariate for the phase of the outbreak each estimate was made in is another
#' option, since the `brms` formula makes it a meta-regression that estimates
#' the residual bias rather than correcting it mechanically.
#'
#' # How a study can report its estimate
#'
#' A study that cannot share its delays should report a vector of summaries
#' with a covariance matrix over them rather than a standard error for each
#' one. This is the recommended format, because it keeps the correlation
#' between the reported quantities. Supply the matrices through `vcov`, named
#' by study. Each must be symmetric positive definite and match the number of
#' rows its study has in `data`, in the order they appear. Such rows need no
#' `n` and no `se`.
#'
#' Three helpers build both parts. [draws_to_multivariate()] takes draws of the
#' quantities a study reports, such as the posterior draws
#' [predict_delay_parameters()] returns. [delays_to_multivariate()] resamples a
#' set of delays. [parameters_to_multivariate()] takes the parameters of a
#' distribution a study fitted, which studies often publish in place of
#' summaries, and converts them to the summaries it implies. Reported
#' parameters are not a `type` here, because the family a study fitted need not
#' be the family being fitted to it.
#'
#' @param data A `data.frame` of published summary estimates.
#'
#' @param study A string giving the column of `data` containing the study
#'  identifier. Defaults to `NULL` which assumes the variable `study` is
#'  present.
#'
#' @param type A string giving the column of `data` containing the summary type.
#'  Each entry must be one of `"mean"`, `"sd"`, or `"quantile"`. Defaults to
#'  `NULL` which assumes the variable `type` is present.
#'
#' @param value A string giving the column of `data` containing the reported
#'  value of the summary. Defaults to `NULL` which assumes the variable `value`
#'  is present.
#'
#' @param se A string giving the column of `data` containing the reported
#'  standard error of the summary. Optional. When supplied it overrides the
#'  standard error implied by the sample size, and takes the row out of the
#'  joint likelihood [as_epidist_meta_model()] otherwise uses for summaries a
#'  study computed from the same delays. It is always on the scale of the
#'  reported `value`, so for a `"quantile"` row it is a standard error for the
#'  reported delay.
#'
#' @param n A string giving the column of `data` containing the number of
#'  delays the summary was computed from. Required unless `se` or a covariance
#'  matrix is supplied.
#'
#' @param vcov A named list of covariance matrices over the summaries each
#'  study reported, named by study. See the section on reporting a covariance
#'  matrix. Optional, and studies without an entry are fitted with their `n` or
#'  `se` as usual.
#'
#' @param p A string giving the column of `data` containing the probability of
#'  a reported quantile. Required for rows with `type` of `"quantile"` and
#'  ignored otherwise.
#'
#' @param pwindow,swindow Strings giving the columns of `data` containing the
#'  primary and secondary event censoring window widths. Default to 1 (daily
#'  reporting) when not supplied.
#'
#' @param relative_obs_time A string giving the column of `data` containing the
#'  observation time relative to the primary event, that is the right
#'  truncation point on the delay scale for a cohort design, or the length of
#'  the collection window for an accrual design. Defaults to `Inf`, meaning no
#'  truncation.
#'
#' @param trunc_adjusted A string giving the column of `data` containing a
#'  logical flag for whether the study corrected for right truncation. Defaults
#'  to `TRUE` where no `relative_obs_time` is supplied and `FALSE` otherwise.
#'
#' @param trunc_design A string giving the column of `data` containing how the
#'  study stopped collecting data, either `"cohort"` (it followed every primary
#'  event for the same `relative_obs_time`) or `"accrual"` (it collected over a
#'  window of that length and stopped at its calendar end). Defaults to
#'  `"cohort"`, and is only used for studies that did not adjust for right
#'  truncation.
#'
#' @param cens_adjusted A string giving the column of `data` containing the
#'  censoring adjustment code (`0`, `1`, `2`, `3`, or `4`, as described
#'  above). Defaults to 0.
#'
#' @param delay_min A string giving the column of `data` containing the
#'  smallest delay the study counted, its left truncation point. Defaults to 0,
#'  meaning the study counted every delay. Must be below the grid cutoff, and
#'  no reported mean or quantile may fall below it.
#'
#' @param growth_rate A string giving the column of `data` containing the
#'  exponential growth rate of primary events during the study period. Defaults
#'  to 0, a uniform primary event within its window. A non-zero rate is
#'  expensive, because the primary censored delay distribution then has no
#'  analytical solution and every evaluation becomes a numerical integration.
#'  Leave it at 0 unless the study period covered enough growth for the
#'  dynamical bias to matter.
#'
#' @param max_delay A string giving the column of `data` containing the delay
#'  beyond which the implied summaries are truncated when building the discrete
#'  grid. Only used when the study adjusted for right truncation. Defaults to
#'  twenty times the largest reported value for the study, rounded up, with a
#'  minimum of ten. Raise it for a long tailed delay, whose implied standard
#'  deviation is biased downwards if the distribution has not decayed by the
#'  cutoff, and lower it to fit faster.
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_estimates_data data.frame
#'
#' @family estimates_data
#' @autoglobal
#' @importFrom checkmate assert_data_frame assert_subset assert_logical
#' @export
#' @examples
#' as_epidist_estimates_data(
#'   data.frame(
#'     study = c("A", "A", "B"),
#'     type = c("mean", "sd", "quantile"),
#'     value = c(7.5, 3.6, 11.2),
#'     p = c(NA, NA, 0.9),
#'     n = c(120, 120, 80),
#'     relative_obs_time = c(20, 20, Inf),
#'     trunc_adjusted = c(FALSE, FALSE, TRUE),
#'     cens_adjusted = c(0, 0, 1)
#'   )
#' )
as_epidist_estimates_data.data.frame <- function(
  data,
  study = NULL,
  type = NULL,
  value = NULL,
  se = NULL,
  n = NULL,
  p = NULL,
  pwindow = NULL,
  swindow = NULL,
  relative_obs_time = NULL,
  trunc_adjusted = NULL,
  trunc_design = NULL,
  cens_adjusted = NULL,
  delay_min = NULL,
  growth_rate = NULL,
  max_delay = NULL,
  vcov = NULL,
  ...
) {
  assert_data_frame(data)

  supplied <- list(
    study, type, value, se, n, p, pwindow, swindow, relative_obs_time,
    trunc_adjusted, trunc_design, cens_adjusted, delay_min, growth_rate,
    max_delay
  )
  valid_inputs <- !vapply(supplied, is.null, logical(1))
  data_tbl <- .rename_columns(
    tibble::as_tibble(data),
    new_names = .estimates_required_cols()[valid_inputs],
    old_names = unlist(supplied)
  )

  for (col in c("study", "type", "value")) {
    if (!hasName(data_tbl, col)) {
      cli::cli_abort("{.var {col}} is NULL but must be provided.")
    }
  }

  data_tbl <- .fill_estimates_defaults(data_tbl)
  data_tbl <- data_tbl[
    c(.estimates_required_cols(), setdiff(
      names(data_tbl), .estimates_required_cols()
    ))
  ]

  data_tbl <- new_epidist_estimates_data(data_tbl)
  data_tbl <- .estimates_set_vcov(data_tbl, vcov)
  assert_epidist(data_tbl)
  return(data_tbl)
}

#' The reported covariance matrices of an `epidist_estimates_data` object
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A named list of covariance matrices, empty when no study reported
#'  one.
#'
#' @keywords internal
.estimates_vcov <- function(data) {
  return(attr(data, "estimates_vcov") %||% list())
}

#' Attach reported covariance matrices to an `epidist_estimates_data` object
#'
#' The matrices are held alongside the data rather than in it, because a
#' covariance matrix spans several rows. Their Cholesky factors are built once
#' by [as_epidist_meta_model()] and passed to Stan, so the sampler never
#' decomposes them.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @param vcov A named list of covariance matrices, or `NULL`.
#'
#' @returns The input with the matrices attached.
#'
#' @keywords internal
.estimates_set_vcov <- function(data, vcov) {
  if (is.null(vcov) || length(vcov) == 0) {
    return(data)
  }
  if (!is.list(vcov) || is.null(names(vcov)) || anyDuplicated(names(vcov))) {
    cli::cli_abort(
      "{.var vcov} must be a list of matrices named by study."
    )
  }
  attr(data, "estimates_vcov") <- lapply(vcov, as.matrix)
  return(data)
}

#' Which rows of an `epidist_estimates_data` object have a covariance matrix
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A logical vector, one entry per row.
#'
#' @keywords internal
.estimates_vcov_rows <- function(data) {
  return(as.character(data$study) %in% names(.estimates_vcov(data)))
}

#' Check the covariance matrices of an `epidist_estimates_data` object
#'
#' Each matrix must cover the rows of its study, be symmetric, and be positive
#' definite, so that it has a Cholesky factor and defines a proper
#' multivariate normal.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
.assert_estimates_vcov <- function(data) {
  supplied <- .estimates_vcov(data)
  if (length(supplied) == 0) {
    return(invisible(NULL))
  }
  study <- as.character(data$study)
  unknown <- setdiff(names(supplied), unique(study))
  if (length(unknown) > 0) {
    cli::cli_abort(paste0(
      "{.var vcov} names {.val {unknown}}, which {?is/are} not among the ",
      "studies in the data."
    ))
  }
  for (name in names(supplied)) {
    matrix_i <- supplied[[name]]
    size <- sum(study == name)
    if (!is.numeric(matrix_i) || nrow(matrix_i) != ncol(matrix_i)) {
      cli::cli_abort(
        "The covariance matrix for {.val {name}} must be square and numeric."
      )
    }
    if (nrow(matrix_i) != size) {
      cli::cli_abort(paste0(
        "The covariance matrix for {.val {name}} is {nrow(matrix_i)} by ",
        "{ncol(matrix_i)} but the study reports {size} summar{?y/ies}."
      ))
    }
    if (!isTRUE(all.equal(matrix_i, t(matrix_i), tolerance = 1e-8))) {
      cli::cli_abort(
        "The covariance matrix for {.val {name}} must be symmetric."
      )
    }
    if (inherits(try(chol(matrix_i), silent = TRUE), "try-error")) {
      cli::cli_abort(paste0(
        "The covariance matrix for {.val {name}} must be positive definite."
      ))
    }
    # Every summary the matrix covers is fitted as one observation, which
    # takes its study metadata from the first of them.
    varies <- vapply(
      data[study == name, .estimates_metadata_cols(), drop = FALSE],
      function(column) {
        return(length(unique(column)) > 1)
      },
      logical(1)
    )
    if (any(varies)) {
      cli::cli_abort(paste0(
        "The summaries {.val {name}} reports with a covariance matrix must ",
        "share their study metadata, but {.var {names(varies)[varies]}} ",
        "{?varies/vary} between them."
      ))
    }
  }
  if (any(!is.na(data$se) & .estimates_vcov_rows(data))) {
    cli::cli_abort(paste0(
      "A study reporting a covariance matrix must not also report a ",
      "{.var se}, because the matrix already gives the uncertainty of every ",
      "summary it reports."
    ))
  }
  return(invisible(NULL))
}

#' Fill in the optional columns of an `epidist_estimates_data` object
#'
#' Applies the documented defaults for study metadata that was not supplied,
#' informing the user about each assumption made on their behalf.
#'
#' @param data A `data.frame` containing at least `study`, `type` and `value`.
#'
#' @returns The input with all of `.estimates_required_cols()` present.
#'
#' @keywords internal
#' @autoglobal
.fill_estimates_defaults <- function(data) {
  if (!hasName(data, "se")) {
    data$se <- NA_real_
  }
  if (!hasName(data, "n")) {
    data$n <- NA_real_
  }
  if (!hasName(data, "p")) {
    data$p <- NA_real_
  }
  for (col in c("pwindow", "swindow")) {
    if (!hasName(data, col)) {
      cli::cli_inform(c(
        i = paste0(
          "No {.var {col}} column supplied, assuming a censoring window of 1 ",
          "(daily reporting) for every study."
        )
      ))
      data[[col]] <- 1
    }
  }
  if (!hasName(data, "relative_obs_time")) {
    cli::cli_inform(c(
      i = paste0(
        "No relative_obs_time column supplied, assuming no observation time ",
        "limit (no right truncation) for every study."
      )
    ))
    data$relative_obs_time <- Inf
  }
  if (!hasName(data, "trunc_adjusted")) {
    data$trunc_adjusted <- is.infinite(data$relative_obs_time)
    cli::cli_inform(c(
      i = paste0(
        "No trunc_adjusted column supplied, assuming studies with a finite ",
        "relative_obs_time did not adjust for right truncation and that all ",
        "others did."
      )
    ))
  }
  data$trunc_adjusted <- as.logical(data$trunc_adjusted)
  if (!hasName(data, "trunc_design")) {
    if (!all(data$trunc_adjusted)) {
      cli::cli_inform(c(
        i = paste0(
          "No trunc_design column supplied, assuming every study that did ",
          "not adjust for right truncation followed a cohort with a common ",
          "observation time rather than accruing primary events up to a ",
          "calendar collection stop."
        )
      ))
    }
    data$trunc_design <- "cohort"
  }
  if (!hasName(data, "cens_adjusted")) {
    cli::cli_inform(c(
      i = paste0(
        "No cens_adjusted column supplied, assuming every study used naive ",
        "integer date differences without a censoring adjustment."
      )
    ))
    data$cens_adjusted <- 0
  }
  if (!hasName(data, "delay_min")) {
    data$delay_min <- 0
  }
  # Studies are often stacked from separate tables, so a study that did not
  # left truncate leaves a gap rather than a zero.
  data$delay_min[is.na(data$delay_min)] <- 0
  if (!hasName(data, "growth_rate")) {
    data$growth_rate <- 0
  }
  if (!hasName(data, "max_delay")) {
    data <- .add_default_max_delay(data)
  }
  data$trunc_design <- as.character(data$trunc_design)
  data$cens_adjusted <- as.integer(data$cens_adjusted)
  data$p[data$type != "quantile"] <- NA_real_
  return(data)
}

#' Add a default grid cutoff to summary estimates
#'
#' The grid used to compute the implied summaries of an unbiased estimand must
#' be finite. Where the study adjusted for right truncation there is no
#' observation time to use, so a cutoff is derived from the reported values.
#' The multiplier is deliberately generous because a cutoff the delay
#' distribution has not decayed by biases the implied standard deviation
#' downwards, by tens of percent for a heavy tailed delay.
#'
#' @param data A `data.frame` containing `study` and `value` columns.
#'
#' @returns The input with an added `max_delay` column.
#'
#' @keywords internal
#' @autoglobal
.add_default_max_delay <- function(data) {
  data <- data |>
    mutate(
      max_delay = pmax(10, ceiling(20 * max(.data$value))),
      .by = "study"
    )
  cli::cli_inform(c(
    i = paste0(
      "No max_delay column supplied, using twenty times the largest reported ",
      "value for each study (minimum 10) as the grid cutoff. Increase this ",
      "if the delay distribution has a long tail, and lower it to speed up ",
      "fitting."
    )
  ))
  return(data)
}

#' Studies whose grid cutoff is short relative to their reported spread
#'
#' The implied summaries of a study that did not adjust for censoring but did
#' adjust for right truncation are computed on a grid running to `max_delay`.
#' A cutoff that the delay distribution has not decayed by biases them
#' downwards. Studies reporting both a mean and a standard deviation allow a
#' rough check of whether the grid reaches far enough.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers with a short cutoff.
#'
#' @keywords internal
.estimates_short_cutoff <- function(data) {
  quadrature <- data$cens_adjusted %in% c(2L, 4L) & data$growth_rate != 0
  uses_grid <- data$trunc_adjusted &
    (data$cens_adjusted %in% c(0L, 3L) | quadrature)
  if (!any(uses_grid)) {
    return(character(0))
  }
  cutoff <- .estimates_grid_cutoff(data)
  studies <- unique(as.character(data$study)[uses_grid])
  short <- vapply(
    studies,
    function(study) {
      rows <- uses_grid & as.character(data$study) == study
      reported_mean <- data$value[rows & data$type == "mean"]
      reported_sd <- data$value[rows & data$type == "sd"]
      if (length(reported_mean) == 0 || length(reported_sd) == 0) {
        return(FALSE)
      }
      return(
        min(cutoff[rows]) < max(reported_mean) + 10 * max(reported_sd)
      )
    },
    logical(1)
  )
  return(studies[short])
}

#' Class constructor for `epidist_estimates_data` objects
#'
#' @param data A data.frame to convert
#'
#' @returns An object of class `epidist_estimates_data`
#'
#' @family estimates_data
#' @export
#' @examples
#' df <- new_epidist_estimates_data(data.frame())
#' class(df)
new_epidist_estimates_data <- function(data) {
  class(data) <- c("epidist_estimates_data", class(data))
  return(data)
}

#' Check if data has the `epidist_estimates_data` class
#'
#' @inheritParams as_epidist_estimates_data
#'
#' @param ... Additional arguments
#'
#' @family estimates_data
#' @export
is_epidist_estimates_data <- function(data, ...) {
  return(inherits(data, "epidist_estimates_data"))
}

#' Assert validity of `epidist_estimates_data` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments
#'
#' @method assert_epidist epidist_estimates_data
#'
#' @family estimates_data
#' @autoglobal
#' @export
assert_epidist.epidist_estimates_data <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = .estimates_required_cols())
  assert_subset(data$type, .estimates_types(), .var.name = "type")
  assert_numeric(data$value, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(data$se, lower = 0)
  assert_numeric(data$n, lower = 1)
  assert_numeric(data$pwindow, lower = 0, any.missing = FALSE)
  assert_numeric(data$swindow, lower = 0, any.missing = FALSE)
  assert_numeric(data$relative_obs_time, lower = 0, any.missing = FALSE)
  assert_logical(data$trunc_adjusted, any.missing = FALSE)
  assert_subset(
    data$trunc_design, .estimates_trunc_designs(),
    .var.name = "trunc_design"
  )
  assert_subset(data$cens_adjusted, 0:4, .var.name = "cens_adjusted")
  assert_numeric(
    data$delay_min,
    lower = 0, any.missing = FALSE, finite = TRUE
  )
  assert_numeric(data$growth_rate, any.missing = FALSE, finite = TRUE)
  assert_numeric(
    data$max_delay,
    lower = 0, any.missing = FALSE, finite = TRUE
  )

  if (any(data$swindow <= 0)) {
    cli::cli_abort("{.var swindow} must be greater than zero.")
  }

  is_quantile <- data$type == "quantile"
  if (any(is_quantile & (is.na(data$p) | data$p <= 0 | data$p >= 1))) {
    cli::cli_abort(paste0(
      "Rows with a {.val quantile} type must have a probability {.var p} ",
      "strictly between 0 and 1."
    ))
  }

  .assert_estimates_vcov(data)

  if (any(is.na(data$n) & is.na(data$se) & !.estimates_vcov_rows(data))) {
    cli::cli_abort(paste0(
      "Every reported summary needs a sample size {.var n}, a reported ",
      "standard error {.var se}, or a covariance matrix in {.var vcov}, so ",
      "that its sampling uncertainty can be quantified."
    ))
  }

  cutoff <- .estimates_grid_cutoff(data)
  if (any(data$delay_min >= cutoff)) {
    cli::cli_abort(paste0(
      "{.var delay_min} must be below the grid cutoff (the observation time, ",
      "or {.var max_delay} where the study adjusted for right truncation)."
    ))
  }

  below <- data$type %in% c("mean", "quantile") & data$value < data$delay_min
  if (any(below)) {
    cli::cli_abort(paste0(
      "A study cannot report a summary below the smallest delay it counted. ",
      "This fails for {.val {unique(as.character(data$study)[below])}}."
    ))
  }

  if (any(!data$trunc_adjusted & is.infinite(data$relative_obs_time))) {
    cli::cli_abort(paste0(
      "Studies that did not adjust for right truncation must have a finite ",
      "{.var relative_obs_time} giving the observation time on the delay ",
      "scale."
    ))
  }

  if (any(cutoff < data$swindow)) {
    cli::cli_abort(paste0(
      "The grid cutoff (the observation time, or {.var max_delay} where the ",
      "study adjusted for right truncation) must be at least as large as ",
      "{.var swindow}."
    ))
  }

  beyond <- is_quantile & data$value >= .estimates_quantile_limit(data)
  if (any(beyond)) {
    cli::cli_abort(paste0(
      "Reported quantiles must fall below the largest delay the study could ",
      "have seen, which is its observation time, or {.var max_delay} where ",
      "the study adjusted for right truncation, and the top of the discrete ",
      "grid where it did not adjust for censoring. This fails for ",
      "{.val {unique(as.character(data$study)[beyond])}}, whose reported ",
      "quantile carries no information about the delay distribution."
    ))
  }

  short <- .estimates_short_cutoff(data)
  if (length(short) > 0) {
    cli::cli_inform(c(
      "!" = paste0(
        "The grid cutoff for {.val {short}} is short relative to the ",
        "reported mean and standard deviation, so the implied summaries for ",
        "{?this study/these studies} will be biased downwards. Increase ",
        "{.var max_delay}."
      )
    ))
  }

  return(invisible(NULL))
}

#' The grid cutoff implied by a set of summary estimates
#'
#' Studies that did not adjust for right truncation are evaluated on a grid
#' running to their observation time. Studies that did are evaluated on a grid
#' running to `max_delay`.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A numeric vector of grid cutoffs.
#'
#' @keywords internal
.estimates_grid_cutoff <- function(data) {
  return(ifelse(
    data$trunc_adjusted, data$max_delay, data$relative_obs_time
  ))
}

#' The largest reported quantile a set of summary estimates can support
#'
#' A quantile reported at or beyond the top of the estimand's support has an
#' implied cumulative probability of one and an implied density of zero, so the
#' delta method conversion of a delay scale standard error hits its floor and
#' the row contributes a constant to the likelihood instead of information.
#' The limit is the top of the discrete grid for a study that did not adjust
#' for censoring, allowing for the half cell the continuity correction adds and
#' for the half window midpoint imputation shifts by, and the grid cutoff
#' otherwise.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A numeric vector of limits, one per row.
#'
#' @keywords internal
.estimates_quantile_limit <- function(data) {
  cutoff <- .estimates_grid_cutoff(data)
  top <- floor(cutoff / data$swindow) * data$swindow
  return(dplyr::case_when(
    data$cens_adjusted == 0L ~ top - data$swindow / 2,
    data$cens_adjusted == 3L ~ top,
    data$cens_adjusted == 4L ~ cutoff - data$pwindow / 2,
    .default = cutoff
  ))
}

.estimates_metadata_cols <- function() {
  return(setdiff(
    .estimates_required_cols(), c("study", "type", "value", "se", "p")
  ))
}

.estimates_types <- function() {
  return(c("mean", "sd", "quantile"))
}

.estimates_trunc_designs <- function() {
  return(c("cohort", "accrual"))
}

.estimates_required_cols <- function() {
  return(c(
    "study",
    "type",
    "value",
    "se",
    "n",
    "p",
    "pwindow",
    "swindow",
    "relative_obs_time",
    "trunc_adjusted",
    "trunc_design",
    "cens_adjusted",
    "delay_min",
    "growth_rate",
    "max_delay"
  ))
}
