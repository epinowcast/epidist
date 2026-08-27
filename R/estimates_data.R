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
#' # What shape a study reported its estimate in
#'
#' This method takes a long table with one row per reported summary. Two other
#' shapes are common enough to have their own entry points, each returning an
#' object this one would.
#'
#' * [epidist_estimates_summaries()] takes one study's summaries in wide form.
#' * [epidist_estimates_parameters()] takes the parameters of a distribution a
#'   study fitted, which studies often publish in place of summaries, and
#'   converts them to the summaries the fitted distribution implies. Reported
#'   parameters are not a `type` here, because the family a study fitted need
#'   not be the family being fitted to it.
#'
#' A study that published draws of its parameters, rather than point summaries
#' of them, can report the correlation between the quantities it reports. Pass
#' the draws to [as_epidist_multivariate()] and the result to
#' [as_epidist_estimates_data()], which is the only route by which a covariance
#' reaches [as_epidist_meta_model()]. Such rows need no `n` and no `se`.
#'
#' Contributions from several studies combine by passing them in a list, which
#' [as_epidist_estimates_data()] binds into one object.
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
#'  delays the summary was computed from. Required unless `se` is supplied.
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
#'  truncation. The accrual weight is exact only where `pwindow` and `swindow`
#'  are equal. With a weekly primary and a daily secondary window, a collection
#'  window of 28 days and a delay of mean 4.6 days, refitting a reported mean
#'  and standard deviation recovers the standard deviation about 6% high. See
#'  `vignette("model")`.
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
  ...
) {
  assert_data_frame(data)

  supplied <- list(
    study = study, type = type, value = value, se = se, n = n, p = p,
    pwindow = pwindow, swindow = swindow,
    relative_obs_time = relative_obs_time, trunc_adjusted = trunc_adjusted,
    trunc_design = trunc_design, cens_adjusted = cens_adjusted,
    delay_min = delay_min, growth_rate = growth_rate, max_delay = max_delay
  )
  valid_inputs <- !vapply(supplied, is.null, logical(1))
  data_tbl <- .rename_columns(
    tibble::as_tibble(data),
    new_names = names(supplied)[valid_inputs],
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
  data_tbl <- .estimates_set_vcov(data_tbl, attr(data, "estimates_vcov"))
  assert_epidist(data_tbl)
  return(data_tbl)
}

#' Combine `epidist_estimates_data` objects from several studies
#'
#' Each element is coerced on its own and the results are bound into one
#' object. Combining is associative, so contributions can be assembled in any
#' order and in any grouping.
#'
#' @inheritParams as_epidist_estimates_data
#'
#' @param ... Passed to the method used for each element.
#'
#' @method as_epidist_estimates_data list
#'
#' @family estimates_data
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' as_epidist_estimates_data(list(
#'   epidist_estimates_summaries("A", mean = 7.5, sd = 3.6, n = 120),
#'   epidist_estimates_summaries("B", mean = 6.9, n = 80)
#' ))
as_epidist_estimates_data.list <- function(data, ...) {
  if (length(data) == 0) {
    cli::cli_abort("{.var data} must hold at least one contribution.")
  }
  parts <- lapply(data, as_epidist_estimates_data, ...)
  return(.estimates_bind(parts))
}

#' Return an `epidist_estimates_data` object unchanged
#'
#' @inheritParams as_epidist_estimates_data
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_estimates_data epidist_estimates_data
#'
#' @family estimates_data
#' @export
#' @examples
#' estimates <- epidist_estimates_summaries("A", mean = 7.5, n = 120)
#' identical(as_epidist_estimates_data(estimates), estimates)
as_epidist_estimates_data.epidist_estimates_data <- function(data, ...) {
  assert_epidist(data)
  return(data)
}

#' Create an `epidist_estimates_data` object from a multivariate representation
#'
#' This is the only route by which a covariance between a study's reported
#' summaries reaches [as_epidist_meta_model()]. The covariance comes from draws
#' of the study's parameters, so it is one the study could have computed, and
#' the summaries it covers are fitted with a multivariate normal likelihood
#' rather than as independent observations.
#'
#' Where `family` is given, the draws hold the natural parameters of a
#' distribution the study fitted. Each draw is pushed through to the summaries
#' the fitted distribution implies, over the range of delays the study could
#' have seen, and the covariance is taken over those. This is the exact version
#' of the delta method [epidist_estimates_parameters()] applies, and it needs
#' no linearisation.
#'
#' Where `family` is `NULL`, the parameters must already be quantities a study
#' reports. Name them `mean`, `sd`, or `q` followed by a probability, such as
#' `q0.25`.
#'
#' Draws over more than one index point are not yet supported for fitting,
#' because the linear predictor would have to vary within one likelihood
#' observation.
#'
#' @inheritParams as_epidist_estimates_data
#'
#' @param study A string naming the study the draws come from.
#'
#' @param family The distribution the study fitted, one of `"lognormal"`,
#'  `"gamma"` or `"weibull"`, where the draws hold its natural parameters.
#'  Defaults to `NULL`, meaning the draws already hold reported summaries.
#'
#' @param moments Which moments to report, any of `"mean"` and `"sd"`. Only
#'  used where `family` is given.
#'
#' @param probs A numeric vector of probabilities to report quantiles at. Only
#'  used where `family` is given.
#'
#' @param mvn_id A string identifying the covariance matrix. Defaults to
#'  `study`, and only needs setting where one study contributes more than one
#'  multivariate object.
#'
#' @param ... Study metadata, as documented in
#'  [as_epidist_estimates_data.data.frame()].
#'
#' @method as_epidist_estimates_data epidist_multivariate
#'
#' @family estimates_data
#' @importFrom tibble tibble
#' @export
#' @examples
#' set.seed(1)
#' draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
#' as_epidist_estimates_data(
#'   as_epidist_multivariate(draws),
#'   study = "site A"
#' )
as_epidist_estimates_data.epidist_multivariate <- function(
  data,
  study,
  family = NULL,
  moments = c("mean", "sd"),
  probs = numeric(0),
  mvn_id = NULL,
  ...
) {
  assert_string(study)
  assert_epidist(data)
  if (length(data$index) > 1) {
    cli::cli_abort(paste0(
      "A multivariate representation spanning {length(data$index)} index ",
      "points cannot be fitted, because the linear predictor would have to ",
      "vary within one likelihood observation. Take a single index point."
    ))
  }
  if (is.null(mvn_id)) {
    mvn_id <- study
  }
  assert_string(mvn_id)
  if (is.null(family)) {
    reported <- .estimates_multivariate_types(data$params)
    value <- unname(data$value)
    covariance <- data$vcov
  } else {
    reported <- .estimates_mvn_summarise(
      data, family, moments, probs, ...
    )
    value <- reported$value
    covariance <- reported$vcov
  }
  rows <- tibble(
    study = study,
    type = reported$type,
    value = value,
    p = reported$p,
    mvn_id = mvn_id,
    ...
  )
  attr(rows, "estimates_vcov") <- stats::setNames(list(covariance), mvn_id)
  return(as_epidist_estimates_data(rows))
}

#' What each element of a multivariate representation reports
#'
#' @param params The parameter names of an `epidist_multivariate` object.
#'
#' @returns A list with the `type` and `p` of each element.
#'
#' @keywords internal
.estimates_multivariate_types <- function(params) {
  type <- rep(NA_character_, length(params))
  p <- rep(NA_real_, length(params))
  moment <- params %in% c("mean", "sd")
  type[moment] <- params[moment]
  reported <- !moment & grepl("^q0?\\.[0-9]+$", params)
  type[reported] <- "quantile"
  p[reported] <- as.numeric(sub("^q", "", params[reported]))
  if (anyNA(type)) {
    cli::cli_abort(c(
      paste0(
        "{.val {params[is.na(type)]}} {?is/are} not {?a quantity/quantities} ",
        "a study reports."
      ),
      i = paste0(
        "Name the parameters {.val mean}, {.val sd}, or {.val q} followed by ",
        "a probability such as {.val q0.25}. Supply {.var family} instead ",
        "where they are the natural parameters of a fitted distribution."
      )
    ))
  }
  return(list(type = type, p = p))
}

#' Summarise draws of a study's fitted parameters
#'
#' @param data An `epidist_multivariate` object holding draws of the natural
#'  parameters of a fitted distribution.
#'
#' @param family The distribution the study fitted.
#'
#' @param moments Which moments to report.
#'
#' @param probs A numeric vector of probabilities to report quantiles at.
#'
#' @param ... Study metadata, used for the range the summaries are taken over.
#'
#' @returns A list with the `type`, `p`, `value` and `vcov` of the summaries.
#'
#' @keywords internal
.estimates_mvn_summarise <- function(
  data, family, moments, probs, ...
) {
  assert_choice(family, names(.estimates_parameter_sets()))
  if (is.null(data$draws)) {
    cli::cli_abort(paste0(
      "Converting the natural parameters of a fitted distribution needs the ",
      "draws themselves, but this object was built from a value and a ",
      "covariance matrix. Use {.fun epidist_estimates_parameters} instead."
    ))
  }
  moments <- .estimates_moments(moments, probs)
  sorted <- .assert_estimates_parameters(family, data$value)
  columns <- match(names(sorted), data$params)
  support <- .estimates_reported_support(...)
  summaries <- t(apply(data$draws[, columns, drop = FALSE], 1, function(draw) {
    return(.estimates_parameter_summary(
      family, stats::setNames(draw, names(sorted)), moments, probs,
      lower = support$lower, cutoff = support$cutoff
    ))
  }))
  covariance <- stats::cov(summaries)
  dimnames(covariance) <- NULL
  .assert_multivariate_definite(covariance)
  return(list(
    type = c(moments, rep("quantile", length(probs))),
    p = c(rep(NA_real_, length(moments)), probs),
    value = colMeans(summaries),
    vcov = covariance
  ))
}

#' Bind coerced contributions into one `epidist_estimates_data` object
#'
#' Covariance matrices are keyed by `mvn_id`, so a key used by two
#' contributions is renamed before the rows are bound.
#'
#' @param parts A list of `epidist_estimates_data` objects.
#'
#' @returns An `epidist_estimates_data` object.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.estimates_bind <- function(parts) {
  if (length(parts) == 1) {
    return(parts[[1]])
  }
  taken <- character(0)
  matrices <- list()
  for (i in seq_along(parts)) {
    supplied <- .estimates_vcov(parts[[i]])
    for (name in names(supplied)) {
      unique_name <- name
      suffix <- 1L
      while (unique_name %in% taken) {
        suffix <- suffix + 1L
        unique_name <- paste0(name, "_", suffix)
      }
      taken <- c(taken, unique_name)
      matrices[[unique_name]] <- supplied[[name]]
      rows <- !is.na(parts[[i]]$mvn_id) & parts[[i]]$mvn_id == name
      parts[[i]]$mvn_id[rows] <- unique_name
    }
  }
  bound <- bind_rows(lapply(parts, function(part) {
    return(tibble::as_tibble(unclass(part)))
  }))
  bound <- new_epidist_estimates_data(bound)
  bound <- .estimates_set_vcov(bound, matrices)
  assert_epidist(bound)
  return(bound)
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
  supplied <- attr(data, "estimates_vcov")
  if (is.null(supplied)) {
    return(list())
  }
  return(supplied)
}

#' Attach reported covariance matrices to an `epidist_estimates_data` object
#'
#' The matrices are held alongside the data rather than in it, because a
#' covariance matrix spans several rows. They are keyed by the `mvn_id` column,
#' which names the multivariate object the rows came from, so one study may
#' contribute more than one. Their Cholesky factors are built once by
#' [as_epidist_meta_model()] and passed to Stan, so the sampler never
#' decomposes them.
#'
#' Only [as_epidist_estimates_data.epidist_multivariate()] writes to this.
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
      "{.var vcov} must be a list of matrices named by {.var mvn_id}."
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
  if (!hasName(data, "mvn_id")) {
    return(rep(FALSE, nrow(data)))
  }
  return(!is.na(data$mvn_id) & data$mvn_id %in% names(.estimates_vcov(data)))
}

#' Check the covariance matrices of an `epidist_estimates_data` object
#'
#' Each matrix must cover the rows sharing its `mvn_id`, be symmetric, and be
#' positive definite, so that it has a Cholesky factor and defines a proper
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
  group <- if (hasName(data, "mvn_id")) data$mvn_id else NA_character_
  unknown <- setdiff(names(supplied), unique(stats::na.omit(group)))
  if (length(unknown) > 0) {
    cli::cli_abort(paste0(
      "A covariance matrix is held for {.val {unknown}}, which {?is/are} not ",
      "among the {.var mvn_id} values in the data."
    ))
  }
  for (name in names(supplied)) {
    matrix_i <- supplied[[name]]
    rows <- !is.na(group) & group == name
    size <- sum(rows)
    if (!is.numeric(matrix_i) || nrow(matrix_i) != ncol(matrix_i)) {
      cli::cli_abort(
        "The covariance matrix for {.val {name}} must be square and numeric."
      )
    }
    if (nrow(matrix_i) != size) {
      cli::cli_abort(paste0(
        "The covariance matrix for {.val {name}} is {nrow(matrix_i)} by ",
        "{ncol(matrix_i)} but it covers {size} summar{?y/ies}."
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
      data[rows, .estimates_metadata_cols(), drop = FALSE],
      function(column) {
        return(length(unique(column)) > 1)
      },
      logical(1)
    )
    if (any(varies)) {
      cli::cli_abort(paste0(
        "The summaries covered by the covariance matrix {.val {name}} must ",
        "share their study metadata, but {.var {names(varies)[varies]}} ",
        "{?varies/vary} between them."
      ))
    }
  }
  if (any(!is.na(data$se) & .estimates_vcov_rows(data))) {
    cli::cli_abort(paste0(
      "A summary covered by a covariance matrix must not also report a ",
      "{.var se}, because the matrix already gives its uncertainty."
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
  if (!hasName(data, "mvn_id")) {
    data$mvn_id <- NA_character_
  }
  data$mvn_id <- as.character(data$mvn_id)
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

#' Studies reporting quantiles on a coarse delay grid
#'
#' A study that summarised interval censored delays without adjusting for
#' censoring (`cens_adjusted` of 0 or 3) reports quantiles of a discrete
#' distribution, which the model interpolates through the mid points of its
#' cells. The reported value is still rounded to that grid, and what the
#' interpolation leaves behind does not shrink with the study sample size. It
#' is a few percent once the reported quantiles sit a few tens of cells above
#' the smallest delay the study counted, and tens of percent when they sit
#' within about ten, which is where this flags them.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers reporting quantiles on a
#'  coarse grid.
#'
#' @keywords internal
.estimates_coarse_quantiles <- function(data) {
  rows <- data$type == "quantile" & data$cens_adjusted %in% c(0L, 3L)
  if (!any(rows)) {
    return(character(0))
  }
  cells <- (data$value - data$delay_min) / data$swindow
  studies <- unique(as.character(data$study)[rows])
  coarse <- vapply(
    studies,
    function(study) {
      keep <- rows & as.character(data$study) == study
      return(max(cells[keep]) < 10)
    },
    logical(1)
  )
  return(studies[coarse])
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
  assert_character(data$mvn_id)

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
      "standard error {.var se}, or a covariance matrix, so that its ",
      "sampling uncertainty can be quantified."
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

  coarse <- .estimates_coarse_quantiles(data)
  if (length(coarse) > 0) {
    cli::cli_inform(c(
      "!" = paste0(
        "The quantiles reported by {.val {coarse}} sit within ten censoring ",
        "windows of the smallest delay {?this study/these studies} counted, ",
        "so the discrete grid barely resolves the delay. A reported quantile ",
        "is rounded to that grid, which can bias the fit by tens of percent ",
        "and does not shrink as {.var n} grows. Check that {.var swindow} is ",
        "the resolution the study worked at, and fit a reported mean and ",
        "standard deviation in preference where one is available."
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
    .estimates_required_cols(),
    c("study", "type", "value", "se", "p", "mvn_id")
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
    "max_delay",
    "mvn_id"
  ))
}
