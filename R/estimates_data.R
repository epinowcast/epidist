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
#'     the secondary date is recorded precisely. With a wide primary window
#'     the shortest delays are reported below zero, so the estimand puts mass
#'     there. Set `delay_min` if the study dropped them.
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
#'   reporting gives windows of 1, weekly reporting gives 7. A fully adjusted
#'   study does not use them and may leave them `NA`.
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
#' # Checks
#'
#' Beyond validating its input, this method runs advisory checks on the
#' summaries and messages about the studies they flag. They run once, when
#' the object is built, and not again when it is passed to
#' [as_epidist_meta_model()]. Each message names the studies concerned, and
#' the row of the input where a single summary is meant, and points here for
#' the reasoning.
#'
#' * **Assumed truncation adjustment.** Where no `trunc_adjusted` column is
#'   supplied, a study with no finite `relative_obs_time` is taken to have
#'   adjusted for right truncation and every other study not to have. Real
#'   time estimates are right truncated unless the study corrected for it,
#'   and reviews rarely record which did, so this is the assumption most
#'   likely to be wrong and is a warning rather than a message.
#' * **Short grid cutoff.** The implied summaries of a study that adjusted
#'   for right truncation but is evaluated on a grid, which is a study with
#'   `cens_adjusted` 0 or 3, or 2 or 4 with a non zero `growth_rate`, run to
#'   `max_delay`. A cutoff the delay distribution has not decayed by biases
#'   them downwards, and the standard deviation most, because the tail beyond
#'   the cutoff carries a share of the second moment out of all proportion to
#'   its mass. A lognormal is matched to what the study reported, through its
#'   mean and standard deviation, or its median and largest quantile above
#'   the median where it reported only quantiles, and the study is flagged
#'   when more than 2% of the second moment of that lognormal lies beyond the
#'   cutoff. That is where the standard deviation on the grid falls about 1%
#'   short, and the shortfall grows with the share. Studies reporting neither
#'   pair are not checked. Raise `max_delay` for the study.
#' * **Coarse quadrature.** The moments and distribution function of a
#'   continuous estimand truncated at the grid cutoff are computed by
#'   Simpson's rule on equally spaced intervals from `delay_min` to the
#'   cutoff, so the node spacing is set by the cutoff and not by the scale of
#'   the delay. The number of intervals is chosen per study so that the
#'   spacing is at most a quarter of the spread the study reported, with
#'   `options(epidist.meta_n_quad)` as its floor and 2000 as a cap the option
#'   lifts when set above it. A study whose cutoff is very long relative to
#'   its spread hits the cap and is left with nodes further apart than that,
#'   so its implied summaries may be inaccurate. This covers a study that did
#'   not adjust for right truncation and used a continuous adjustment
#'   (`cens_adjusted` 1, 2 or 4), a study that did adjust but whose primary
#'   events were not uniform within their window (`cens_adjusted` 2 or 4 with
#'   a non zero `growth_rate`), and the quantiles of a study reporting a
#'   covariance matrix, which are read off the same nodes. Raise the option
#'   above the cap before building the model data, or lower `max_delay`.
#' * **Coarse quantiles.** A study that summarised interval censored delays
#'   without adjusting for censoring (`cens_adjusted` 0 or 3) reports
#'   quantiles of a discrete distribution, which the model interpolates
#'   through the mid points of its cells. The reported value is still rounded
#'   to that grid, and what the interpolation leaves behind does not shrink
#'   with the study sample size. It is a few percent once a reported quantile
#'   sits a few tens of cells above the smallest delay the study counted, and
#'   tens of percent when it sits within about ten. A study is flagged on its
#'   smallest reported quantile, the one nearest that edge of the grid,
#'   because the residual on that quantile is what biases the fitted spread
#'   even when the larger quantiles of the same study sit well up the grid.
#'   Check that `swindow` is the resolution the study worked at. A reported
#'   mean and standard deviation of the same delays do not carry this
#'   residual, so fit them in preference where the study gives them.
#' * **Several integer day quantiles from a large study.** A quantile of
#'   delays counted in whole censoring windows is a discrete statistic, and
#'   the information it carries about the delay distribution saturates once
#'   the binomial spread of the crossing point of the empirical distribution
#'   function is narrower than a window. A single such quantile is fitted as
#'   the exact crossing event, but several are still fitted with the
#'   multinomial on the continuity corrected distribution function, whose
#'   claimed precision keeps growing with the sample size. It is calibrated
#'   at around thirty delays and overconfident from around a hundred, so a
#'   study reporting two or more such quantiles from more than 100 delays is
#'   flagged and will be weighted too heavily. Fit a reported mean and
#'   standard deviation instead where one is available.
#' * **Heavy tailed standard deviation.** The sampling standard error of a
#'   reported standard deviation is \eqn{\sigma \sqrt{(\kappa - 1) / (4 n)}},
#'   with \eqn{\kappa} the kurtosis of the delays. The normal approximation
#'   behind it holds while that relative standard error is below about a
#'   quarter. Above it the sampling distribution of a sample standard
#'   deviation is far from normal, the asymptotic standard error overstates
#'   its spread by up to two times, and the joint likelihood of a mean and
#'   standard deviation pair is biased by about a standard error. The
#'   kurtosis is taken from the reported mean and standard deviation under a
#'   lognormal delay, which is a plausible tail for a delay of that
#'   coefficient of variation. Where the study reports quantiles inside the
#'   body of the distribution, those are safer to fit.
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
#'  reporting) when not supplied. A fully adjusted study (`cens_adjusted`
#'  code 1) does not use them, because its estimand is the continuous delay
#'  distribution itself, so its rows may leave them `NA`. Every other code
#'  needs them.
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
#'  A study assumed to have adjusted is warned about, because real time
#'  estimates are right truncated unless the study corrected for it and
#'  reviews rarely record which studies did. Supply the column to say so
#'  yourself.
#'
#' @param trunc_design A string giving the column of `data` containing how the
#'  study stopped collecting data, either `"cohort"` (it followed every primary
#'  event for the same `relative_obs_time`) or `"accrual"` (it collected over a
#'  window of that length and stopped at its calendar end). Defaults to
#'  `"cohort"`, and is only used for studies that did not adjust for right
#'  truncation. The accrual weight on the grid of a study that did not adjust
#'  for censoring is exact whenever `relative_obs_time` is a multiple of
#'  `pwindow`, for any `swindow`. The weight used for the uniform single
#'  interval approximation is exact only for a narrow `pwindow`, and puts the
#'  implied mean about 3% high with a weekly primary window, a collection
#'  window of 28 days, a delay of mean 4.6 days and a growth rate of 0.2. See
#'  `vignette("model")`.
#'
#' @param cens_adjusted A string giving the column of `data` containing the
#'  censoring adjustment code (`0`, `1`, `2`, `3`, or `4`, as described
#'  above). Defaults to 0.
#'
#' @param delay_min A string giving the column of `data` containing the
#'  smallest delay the study counted, its left truncation point, on the scale
#'  the study reported. Defaults to 0, meaning the study counted every delay,
#'  which for `cens_adjusted` code 4 includes any delay reported below zero.
#'  Must be below the grid cutoff, and no reported mean or quantile may fall
#'  below it.
#'
#' @param growth_rate A string giving the column of `data` containing the
#'  exponential growth rate of primary events during the study period. Defaults
#'  to 0. It plays two roles. Within each primary window it tilts the primary
#'  event towards the end of the window, which for a daily window is
#'  negligible. Under `trunc_design = "accrual"` it also weights the follow up
#'  available to each delay, which is the dynamical bias of a growing epidemic
#'  and can move the implied mean by a day or more. A non-zero rate is
#'  expensive, because the primary censored delay distribution then has no
#'  analytical solution and every evaluation becomes a numerical integration.
#'  Leave it at 0 unless the study accrued cases over a period of growth. It
#'  is a known quantity here, taken from the study. For individual level data
#'  the same rate is estimated instead, as the `pgrowth` parameter of
#'  `primary = "expgrowth"` in [as_epidist_marginal_model()]. See
#'  `vignette("primary-events")`.
#'
#' @param max_delay A string giving the column of `data` containing the delay
#'  beyond which the implied summaries are truncated when building the discrete
#'  grid. Only used when the study adjusted for right truncation. Defaults to
#'  the delay beyond which one percent of the second moment of a lognormal
#'  matched to the study's summaries lies, through its mean and standard
#'  deviation, or its median and largest quantile above the median where it
#'  reported only quantiles. The lognormal is used whatever family is fitted
#'  later, because the family is not known when the data are built and, for
#'  the same mean and standard deviation, its tail is heavier than the
#'  gamma's or the Weibull's, so the cutoff is far enough for those families
#'  and only longer than they need. That is the yardstick of the short cutoff check
#'  in the Checks section, so the default never trips it where it binds. It
#'  is rounded up to a whole number of secondary windows, with a minimum of
#'  ten and a maximum of twenty times the largest reported value, because for
#'  a heavy tail one percent of the second moment lies thousands of delays
#'  out, and is five times the largest reported value where nothing can be
#'  matched. Raise
#'  it for a delay with a longer tail than a lognormal, whose implied standard
#'  deviation is biased downwards if the distribution has not decayed by the
#'  cutoff, and lower it to fit faster. A message names the studies whose
#'  cutoff is too short, see the Checks section.
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
  .estimates_advise(data_tbl)
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
#'   epidist_estimates_summaries(
#'     "A",
#'     mean = 7.5, sd = 3.6, n = 120, trunc_adjusted = TRUE
#'   ),
#'   epidist_estimates_summaries(
#'     "B",
#'     mean = 6.9, n = 80, relative_obs_time = 20, trunc_adjusted = FALSE
#'   )
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
#' An object carrying the class has already been checked, and the `epidist_data`
#' methods re-check it whenever it is modified, so nothing is done here.
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
#' estimates <- epidist_estimates_summaries(
#'   "A",
#'   mean = 7.5, n = 120, trunc_adjusted = TRUE
#' )
#' identical(as_epidist_estimates_data(estimates), estimates)
as_epidist_estimates_data.epidist_estimates_data <- function(data, ...) {
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
#'   study = "site A",
#'   trunc_adjusted = TRUE,
#'   cens_adjusted = 1
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
  n_summary <- length(moments) + length(probs)
  if (n_summary > length(sorted)) {
    cli::cli_abort(c(
      paste0(
        "{n_summary} summaries were requested from a {.val {family}} fit ",
        "with {length(sorted)} parameters, but summaries of a fit are ",
        "deterministic functions of its parameters, so at most ",
        "{length(sorted)} of them have a non singular covariance."
      ),
      i = paste0(
        "Report at most {length(sorted)} summaries, for example a mean and a ",
        "standard deviation, or two quantiles."
      )
    ))
  }
  columns <- match(names(sorted), data$params)
  support <- .estimates_reported_support(...)
  summaries <- t(apply(data$draws[, columns, drop = FALSE], 1, function(draw) {
    return(.estimates_parameter_summary(
      family, stats::setNames(draw, names(sorted)), moments, probs,
      lower = support$lower, cutoff = support$cutoff
    ))
  }))
  if (!all(is.finite(summaries))) {
    cli::cli_abort(paste0(
      "Some draws imply a summary that is not finite. Every draw must be a ",
      "valid set of {.val {family}} parameters."
    ))
  }
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
    assumed <- unique(as.character(data$study)[data$trunc_adjusted])
    if (length(assumed) > 0) {
      # Taking a study as truncation adjusted is the assumption most likely
      # to be wrong, so it is a warning rather than a message.
      cli::cli_warn(c(
        "!" = paste0(
          "No trunc_adjusted column supplied, so {.val {assumed}} ",
          "{?is/are} assumed to have adjusted for right truncation, having ",
          "no finite relative_obs_time, and any other study not to have. ",
          "Supply trunc_adjusted to say which studies did."
        ),
        .estimates_checks_pointer()
      ))
    } else {
      cli::cli_inform(c(
        i = paste0(
          "No trunc_adjusted column supplied, assuming every study did not ",
          "adjust for right truncation because each has a finite ",
          "relative_obs_time. Supply trunc_adjusted to say which did."
        ),
        .estimates_checks_pointer()
      ))
    }
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
  assert_integerish(
    data$cens_adjusted,
    lower = 0, upper = 4, any.missing = FALSE, .var.name = "cens_adjusted"
  )
  data$cens_adjusted <- as.integer(data$cens_adjusted)
  data$p[data$type != "quantile"] <- NA_real_
  return(data)
}

#' Add a default grid cutoff to summary estimates
#'
#' The grid used to compute the implied summaries of an unbiased estimand must
#' be finite. Where the study adjusted for right truncation there is no
#' observation time to use, so a cutoff is derived from the reported values.
#' A lognormal is matched to what each study reported by
#' [.estimates_lnorm_match()], and the cutoff is the delay beyond which one
#' percent of its second moment lies, which for parameters `meanlog` and
#' `sdlog` is `exp(meanlog + 2 * sdlog^2 + sdlog * qnorm(0.99))`. This is
#' the same yardstick as the short cutoff check of
#' [.estimates_short_cutoff()], which fires at two percent, so the default
#' never trips it. The cutoff is rounded up to a whole number of secondary
#' windows with a floor of ten. Where nothing can be matched, which is a
#' study reporting a single quantile or a mean with a standard error, the
#' cutoff is five times the largest reported value.
#'
#' @param data A `data.frame` containing `study`, `type`, `value`, `p` and
#'  `swindow` columns.
#'
#' @returns The input with an added `max_delay` column.
#'
#' @keywords internal
.add_default_max_delay <- function(data) {
  studies <- as.character(data$study)
  cutoff <- vapply(
    unique(studies),
    function(study) {
      rows <- studies == study
      lnorm <- .estimates_lnorm_match(data, rows)
      if (is.null(lnorm)) {
        delay <- 5 * max(data$value[rows])
      } else {
        # For a heavy tail one percent of the second moment lies thousands
        # of delays out, so the grid is capped at twenty times the largest
        # reported value.
        delay <- min(
          exp(
            lnorm$meanlog + 2 * lnorm$sdlog^2 +
              lnorm$sdlog * stats::qnorm(0.99)
          ),
          20 * max(data$value[rows])
        )
      }
      # A fully adjusted study may leave its windows NA, and an invalid
      # window is left for the checks to report.
      swindow <- data$swindow[rows]
      swindow <- swindow[is.finite(swindow) & swindow > 0]
      swindow <- ifelse(length(swindow) > 0, max(swindow), 1)
      return(ceiling(max(delay, 10) / swindow) * swindow)
    },
    numeric(1)
  )
  data$max_delay <- unname(cutoff[match(studies, names(cutoff))])
  cli::cli_inform(c(
    i = paste0(
      "No max_delay column supplied, using the delay beyond which 1% of the ",
      "second moment of a lognormal matched to each study's summaries lies ",
      "(at least 10 and at most twenty times the largest reported value, ",
      "in whole secondary windows) as the grid cutoff, or five times the ",
      "largest reported value where nothing can be matched. ",
      "Raise it if the delay has a longer tail than that, and lower it to ",
      "speed up fitting."
    )
  ))
  return(data)
}

#' Studies whose grid cutoff is short relative to their reported tail
#'
#' The implied summaries of a study that did not adjust for censoring but did
#' adjust for right truncation are computed on a grid running to `max_delay`.
#' A cutoff that the delay distribution has not decayed by biases them
#' downwards, and the standard deviation most, because the tail beyond the
#' cutoff carries a share of the second moment out of all proportion to its
#' mass. A lognormal is matched to what each study reported, through its mean
#' and standard deviation, or its median and largest quantile above the
#' median where it reported only quantiles, and the study is flagged when
#' more than 2% of the second moment of that lognormal lies beyond the
#' cutoff. That is where the standard deviation on the grid falls about 1%
#' short, and the shortfall grows with the share. Studies reporting neither
#' pair are not checked.
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
      lnorm <- .estimates_lnorm_match(data, rows)
      if (is.null(lnorm)) {
        return(FALSE)
      }
      beyond <- stats::pnorm(
        (log(min(cutoff[rows])) - lnorm$meanlog - 2 * lnorm$sdlog^2) /
          lnorm$sdlog,
        lower.tail = FALSE
      )
      return(beyond > 0.02)
    },
    logical(1)
  )
  return(studies[short])
}

#' A lognormal matched to the summaries a study reported
#'
#' Matches a lognormal to a reported mean and standard deviation by its
#' moments. Where the study reported only quantiles, its median is the
#' location and its largest quantile above the median, at the level the
#' study reported it, sets the scale. A study reporting neither pair, or
#' one whose quantiles do not increase, gives `NULL`.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @param rows A logical vector selecting the rows of one study.
#'
#' @returns A list with `meanlog` and `sdlog` elements, or `NULL`.
#'
#' @keywords internal
.estimates_lnorm_match <- function(data, rows) {
  reported_mean <- data$value[rows & data$type == "mean"]
  reported_sd <- data$value[rows & data$type == "sd"]
  if (length(reported_mean) > 0 && length(reported_sd) > 0) {
    variance_log <- log1p((max(reported_sd) / max(reported_mean))^2)
    return(list(
      meanlog = log(max(reported_mean)) - variance_log / 2,
      sdlog = sqrt(variance_log)
    ))
  }
  # The match runs before the quantile probabilities are validated, so a
  # missing one is treated as no quantile.
  quantiles <- rows & data$type == "quantile" & !is.na(data$p)
  reported_median <- data$value[quantiles & data$p == 0.5]
  upper <- quantiles & data$p > 0.5
  if (length(reported_median) == 0 || !any(upper)) {
    return(NULL)
  }
  largest <- which(upper)[which.max(data$value[upper])]
  sdlog <- (log(data$value[largest]) - log(max(reported_median))) /
    stats::qnorm(data$p[largest])
  if (!is.finite(sdlog) || sdlog <= 0) {
    return(NULL)
  }
  return(list(meanlog = log(max(reported_median)), sdlog = sdlog))
}

#' The spread each study reported, as a proxy for its delay standard deviation
#'
#' The quadrature a summary row uses has to resolve the delay distribution,
#' whose scale is only known once the model is fitted. The spread the study
#' itself reported stands in for it. That is its reported standard deviation
#' where it gave one, the range of its reported quantiles divided by the same
#' range of a standard normal where it gave two or more, and otherwise a
#' quarter of the smallest location it reported, which is the coefficient of
#' variation below which a delay is narrow. The proxy only has to be within a
#' factor of two or so, because the resolution is chosen well inside where
#' Simpson's rule converges.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A numeric vector of spreads, one per row.
#'
#' @keywords internal
.estimates_spread <- function(data) {
  studies <- as.character(data$study)
  spread <- vapply(
    unique(studies),
    function(study) {
      rows <- studies == study
      sds <- data$value[rows & data$type == "sd"]
      if (length(sds) > 0) {
        return(min(sds))
      }
      reported <- rows & data$type == "quantile"
      p <- data$p[reported]
      value <- data$value[reported]
      if (sum(reported) >= 2 && max(p) > min(p) && max(value) > min(value)) {
        return(
          (max(value) - min(value)) /
            (stats::qnorm(max(p)) - stats::qnorm(min(p)))
        )
      }
      location <- data$value[rows & data$type %in% c("mean", "quantile")]
      location <- location[location > 0]
      if (length(location) == 0) {
        # A fully adjusted study may leave its windows NA.
        narrowest <- min(data$swindow[rows], na.rm = TRUE)
        return(ifelse(is.finite(narrowest), narrowest, 1))
      }
      return(min(location) / 4)
    },
    numeric(1)
  )
  return(unname(spread[match(studies, names(spread))]))
}

#' The number of quadrature intervals each summary row is evaluated on
#'
#' The moments and distribution function of a continuous estimand that is
#' truncated at the grid cutoff are computed by Simpson's rule on equally
#' spaced intervals running from `delay_min` to the cutoff, so the node
#' spacing is set by the cutoff and not by the scale of the delay. A fixed
#' number of intervals leaves a narrow delay unresolved on a wide grid,
#' which pins its implied kurtosis at its floor and can put its implied
#' standard deviation out by a factor of two. The number is therefore chosen
#' per study so that the spacing is at most a quarter of the spread the study
#' reported, see [.estimates_spread()], with `options(epidist.meta_n_quad)`
#' as its floor and [.meta_n_quad_max()] as its cap unless the option is set
#' above it. It is even, because the quadrature uses Simpson's rule.
#'
#' Every row gets a number, including rows on the discrete grid, which do
#' not use it.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns An integer vector of interval counts, one per row.
#'
#' @keywords internal
.estimates_n_quad <- function(data) {
  span <- .estimates_grid_cutoff(data) - data$delay_min
  needed <- 4 * ceiling(span / .estimates_spread(data))
  floor_n <- .meta_n_quad()
  cap <- max(.meta_n_quad_max(), floor_n)
  n_quad <- pmin(pmax(needed, floor_n), cap)
  n_quad <- n_quad + n_quad %% 2
  return(as.integer(n_quad))
}

#' Studies whose quadrature nodes are far apart relative to their delays
#'
#' The number of quadrature intervals of [.estimates_n_quad()] is capped, so
#' a study whose grid cutoff is very long relative to the spread it reported
#' is left with nodes further apart than a quarter of that spread. This
#' covers a study that did not adjust for right truncation and used a
#' continuous adjustment (`cens_adjusted` of 1, 2 or 4), a study that did
#' adjust but whose primary events were not uniform within their window
#' (`cens_adjusted` of 2 or 4 with a non zero `growth_rate`), and the
#' quantile members of a covariance matrix group, which are read off the
#' same nodes.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers whose quadrature is
#'  coarse.
#'
#' @keywords internal
.estimates_coarse_quadrature <- function(data) {
  continuous <- data$cens_adjusted %in% c(1L, 2L, 4L)
  quadrature <- continuous & (
    !data$trunc_adjusted |
      (data$cens_adjusted != 1L & data$growth_rate != 0) |
      (data$type == "quantile" & .estimates_vcov_rows(data))
  )
  if (!any(quadrature)) {
    return(character(0))
  }
  spacing <- (.estimates_grid_cutoff(data) - data$delay_min) /
    .estimates_n_quad(data)
  coarse <- quadrature & spacing > .estimates_spread(data) / 4
  return(unique(as.character(data$study)[coarse]))
}

#' The smallest quantile of each study on a coarse delay grid
#'
#' A study that summarised interval censored delays without adjusting for
#' censoring (`cens_adjusted` of 0 or 3) reports quantiles of a discrete
#' distribution, which the model interpolates through the mid points of its
#' cells. The reported value is still rounded to that grid, and what the
#' interpolation leaves behind does not shrink with the study sample size. It
#' is a few percent once a reported quantile sits a few tens of cells above
#' the smallest delay the study counted, and tens of percent when it sits
#' within about ten. A study is flagged on its smallest reported quantile,
#' the one nearest that edge of the grid, because the residual on that
#' quantile is what biases the fitted spread even when the larger quantiles
#' of the same study sit well up the grid. A reported mean and standard
#' deviation of the same delays do not carry this residual, so they should
#' be fitted in preference where the study gives them.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A logical vector, one entry per row, marking the smallest
#'  reported quantile of each flagged study.
#'
#' @keywords internal
.estimates_coarse_rows <- function(data) {
  rows <- data$type == "quantile" & data$cens_adjusted %in% c(0L, 3L)
  flagged <- rep(FALSE, nrow(data))
  cells <- (data$value - data$delay_min) / data$swindow
  studies <- as.character(data$study)
  for (study in unique(studies[rows])) {
    keep <- which(rows & studies == study)
    smallest <- keep[which.min(cells[keep])]
    flagged[smallest] <- cells[smallest] < 10
  }
  return(flagged)
}

#' Studies reporting quantiles on a coarse delay grid
#'
#' The studies flagged by [.estimates_coarse_rows()].
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers reporting quantiles on a
#'  coarse grid.
#'
#' @keywords internal
.estimates_coarse_quantiles <- function(data) {
  flagged <- .estimates_coarse_rows(data)
  return(unique(as.character(data$study)[flagged]))
}

#' Large studies reporting several quantiles of integer day delays
#'
#' A quantile of delays counted in whole censoring windows is a discrete
#' statistic, and the information it carries about the delay distribution
#' saturates once the binomial spread of the crossing point of the empirical
#' distribution function is narrower than a window. A single such quantile is
#' fitted as the exact crossing event, but several are still fitted with the
#' multinomial on the continuity corrected distribution function, whose
#' claimed precision keeps growing with the sample size. It is calibrated at
#' around thirty delays and overconfident from around a hundred, so studies
#' above that are flagged.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers.
#'
#' @keywords internal
.estimates_overconfident_sets <- function(data) {
  rows <- data$type == "quantile" & data$cens_adjusted %in% c(0L, 3L) &
    !is.na(data$n) & data$n > 100 & is.na(data$se) &
    !.estimates_vcov_rows(data)
  if (!any(rows)) {
    return(character(0))
  }
  studies <- unique(as.character(data$study)[rows])
  several <- vapply(
    studies,
    function(study) {
      return(sum(rows & as.character(data$study) == study) >= 2)
    },
    logical(1)
  )
  return(studies[several])
}

#' Reported standard deviations with a heavy tailed sampling error
#'
#' The sampling standard error of a reported standard deviation is
#' \eqn{\sigma \sqrt{(\kappa - 1) / (4 n)}}, with \eqn{\kappa} the kurtosis
#' of the delays. The normal approximation behind it holds while that
#' relative standard error is below about a quarter. Above it the sampling
#' distribution of a sample standard deviation is far from normal, the
#' asymptotic standard error overstates its spread by up to two times, and
#' the joint likelihood of a mean and standard deviation pair is biased by
#' about a standard error. The kurtosis is taken from the reported mean and
#' standard deviation under a lognormal delay, which is a plausible tail for
#' a delay of that coefficient of variation.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A logical vector, one entry per row, marking each reported
#'  standard deviation with a heavy tailed sampling error.
#'
#' @keywords internal
.estimates_heavy_tail_rows <- function(data) {
  sds <- data$type == "sd" & !is.na(data$n) & is.na(data$se) &
    !.estimates_vcov_rows(data)
  flagged <- rep(FALSE, nrow(data))
  studies <- as.character(data$study)
  for (study in unique(studies[sds])) {
    rows <- studies == study
    reported_mean <- data$value[rows & data$type == "mean"]
    if (length(reported_mean) == 0) {
      next
    }
    keep <- which(sds & rows)
    variance_log <- log1p((data$value[keep] / max(reported_mean))^2)
    kurtosis <- exp(4 * variance_log) + 2 * exp(3 * variance_log) +
      3 * exp(2 * variance_log) - 3
    relative_se <- sqrt((kurtosis - 1) / (4 * data$n[keep]))
    flagged[keep] <- relative_se > 0.25
  }
  return(flagged)
}

#' Studies whose reported standard deviation has a heavy tailed sampling error
#'
#' The studies flagged by [.estimates_heavy_tail_rows()].
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns A character vector of study identifiers.
#'
#' @keywords internal
.estimates_heavy_tail_sd <- function(data) {
  flagged <- .estimates_heavy_tail_rows(data)
  return(unique(as.character(data$study)[flagged]))
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
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$swindow, lower = 0)
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

  # No estimand of a fully adjusted study reads the censoring windows, so
  # they may be left NA there. Every other code uses them.
  uses_window <- data$cens_adjusted != 1L
  for (col in c("pwindow", "swindow")) {
    absent <- uses_window & is.na(data[[col]])
    if (any(absent)) {
      cli::cli_abort(paste0(
        "{.var {col}} is needed for a study with {.var cens_adjusted} code ",
        "0, 2, 3 or 4, but is missing for ",
        "{.val {unique(as.character(data$study)[absent])}}. Only a fully ",
        "adjusted study (code 1) may leave it {.val NA}."
      ))
    }
  }

  if (any(data$swindow <= 0, na.rm = TRUE)) {
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
  # Code 4 anchors the primary event at the midpoint of its window, so a
  # study that dropped delays below delay_min left truncated its base
  # estimand at delay_min + pwindow / 2, which has to sit below the cutoff
  # as well. See .meta_cens_lower().
  moved <- data$cens_adjusted == 4L & data$delay_min > 0 &
    data$delay_min + data$pwindow / 2 >= cutoff
  if (any(moved)) {
    cli::cli_abort(paste0(
      "A study using {.code cens_adjusted = 4} counted delays from half a ",
      "{.var pwindow} above {.var delay_min} on the underlying scale, which ",
      "must be below the grid cutoff. This fails for ",
      "{.val {unique(as.character(data$study)[moved])}}."
    ))
  }

  below <- data$type %in% c("mean", "quantile") & data$value < data$delay_min
  if (any(below)) {
    cli::cli_abort(paste0(
      "A study cannot report a summary below the smallest delay it counted. ",
      "This fails for {.val {unique(as.character(data$study)[below])}}."
    ))
  }

  se_zero <- !is.na(data$se) & data$se <= 0
  if (any(se_zero)) {
    cli::cli_abort(paste0(
      "A reported standard error {.var se} must be greater than zero. This ",
      "fails for {.val {unique(as.character(data$study)[se_zero])}}."
    ))
  }

  sd_zero <- data$type == "sd" & data$value <= 0
  if (any(sd_zero)) {
    cli::cli_abort(paste0(
      "A reported standard deviation must be greater than zero. This fails ",
      "for {.val {unique(as.character(data$study)[sd_zero])}}."
    ))
  }

  if (any(!data$trunc_adjusted & is.infinite(data$relative_obs_time))) {
    cli::cli_abort(paste0(
      "Studies that did not adjust for right truncation must have a finite ",
      "{.var relative_obs_time} giving the observation time on the delay ",
      "scale."
    ))
  }

  beyond_obs <- !data$trunc_adjusted & data$type == "mean" &
    data$value >= data$relative_obs_time
  if (any(beyond_obs)) {
    cli::cli_abort(paste0(
      "A study that did not adjust for right truncation cannot report a ",
      "mean at or beyond its observation time, because every delay it saw ",
      "fell below it. This fails for ",
      "{.val {unique(as.character(data$study)[beyond_obs])}}."
    ))
  }

  if (any(cutoff < data$swindow, na.rm = TRUE)) {
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

  return(invisible(NULL))
}

#' Advise on summary estimates the meta model will fit poorly
#'
#' Runs the advisory checks on a freshly built `epidist_estimates_data`
#' object and messages about each study they flag. They run once, here, when
#' the object is first built, rather than in [assert_epidist()], so that
#' passing the finished object on to [as_epidist_meta_model()] does not
#' repeat them. The checks are described in the Checks section of
#' [as_epidist_estimates_data.data.frame()].
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @returns `NULL`, invisibly, called for the messages it may raise.
#'
#' @keywords internal
.estimates_advise <- function(data) {
  short <- .estimates_short_cutoff(data)
  if (length(short) > 0) {
    cli::cli_inform(c(
      "!" = paste0(
        "The grid cutoff {.var max_delay} for {.val {short}} is short ",
        "relative to the tail {?it/they} reported, so the implied standard ",
        "deviation will be biased downwards. Raise {.var max_delay} for ",
        "{?this study/these studies}."
      ),
      .estimates_checks_pointer()
    ))
  }

  coarse_quadrature <- .estimates_coarse_quadrature(data)
  if (length(coarse_quadrature) > 0) {
    cap <- max(.meta_n_quad_max(), .meta_n_quad())
    cli::cli_inform(c(
      "!" = paste0(
        "The quadrature for {.val {coarse_quadrature}} needs more than ",
        "{cap} intervals to resolve the spread {cli::qty(coarse_quadrature)}",
        "{?it/they} reported, so the implied summaries may be inaccurate. ",
        "Raise {.code options(epidist.meta_n_quad)} above {cap} or lower ",
        "{.var max_delay}."
      ),
      .estimates_checks_pointer()
    ))
  }

  coarse_rows <- .estimates_coarse_rows(data)
  if (any(coarse_rows)) {
    flagged <- .estimates_row_labels(data, coarse_rows)
    cli::cli_inform(c(
      "!" = paste0(
        "The smallest quantile reported by {flagged} sits within ten ",
        "censoring windows of the smallest delay counted, so the discrete ",
        "grid barely resolves it. Check {.var swindow}, and fit a reported ",
        "mean and standard deviation instead where the study gives them."
      ),
      .estimates_checks_pointer()
    ))
  }

  overconfident <- .estimates_overconfident_sets(data)
  if (length(overconfident) > 0) {
    cli::cli_inform(c(
      "!" = paste0(
        "{.val {overconfident}} report{?s/} several quantiles of integer day ",
        "delays from more than 100 delays, so the joint quantile likelihood ",
        "is overconfident and weights {?it/them} too heavily. Fit a reported ",
        "mean and standard deviation instead where one is available."
      ),
      .estimates_checks_pointer()
    ))
  }

  heavy_rows <- .estimates_heavy_tail_rows(data)
  if (any(heavy_rows)) {
    flagged <- .estimates_row_labels(data, heavy_rows)
    cli::cli_inform(c(
      "!" = paste0(
        "The standard deviation reported by {flagged} has a relative ",
        "standard error above 0.25 under the lognormal tail its mean ",
        "implies, so its sampling likelihood cannot be trusted. Fit ",
        "quantiles from the body of the distribution instead where the ",
        "study reports them."
      ),
      .estimates_checks_pointer()
    ))
  }

  return(invisible(NULL))
}

#' Label flagged summary rows by study and position in the input
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @param rows A logical vector selecting the rows to label.
#'
#' @returns A character vector such as `"A" (row 2)`, one entry per selected
#'  row.
#'
#' @keywords internal
.estimates_row_labels <- function(data, rows) {
  return(sprintf(
    "\"%s\" (row %d)", as.character(data$study)[rows], which(rows)
  ))
}

#' The pointer every advisory message ends with
#'
#' @returns A named character vector for [cli::cli_inform()].
#'
#' @keywords internal
.estimates_checks_pointer <- function() {
  return(c(
    i = "See the Checks section of {.code ?as_epidist_estimates_data}."
  ))
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
