#' Simulate the summaries a published study would have reported
#'
#' Applies the observation and estimation procedure of one study to a
#' simulated line list and returns the summaries that study would have
#' published, with the metadata [as_epidist_estimates_data()] needs to
#' forward model them. The line list must carry the exact event times that
#' [simulate_dates()] keeps when `keep_times = TRUE`, because the censoring
#' adjustments a study may have used differ in how much of each event time
#' they saw.
#'
#' The study measures each delay as its censoring adjustment code says, drops
#' the cases its truncation would have hidden from it, drops delays below
#' `delay_min`, takes a sample of `n` cases and summarises them. Each step
#' follows the estimand the meta model uses for that code, see
#' `vignette("model")`.
#'
#' The measured delay is, by `cens_adjusted` code:
#'
#' * 0, the difference between the lower edges of the secondary and primary
#'   windows, which is the integer date difference of a daily line list.
#' * 1, the exact delay between the two events.
#' * 2, the exact secondary time less the lower edge of the primary window,
#'   the uniform single interval approximation.
#' * 3, the date difference of code 0 plus half a secondary window, midpoint
#'   imputation.
#' * 4, the exact secondary time less the midpoint of the primary window.
#'
#' A study that did not adjust for right truncation sees a case only if its
#' delay had completed by its observation time. Under a `"cohort"` design the
#' delay the observation time bounds is the underlying one, so a study on the
#' discrete grid (codes 0 and 3) keeps a case only if the whole window its
#' delay fell in is below `relative_obs_time`, and a code 4 study keeps a
#' case if its code 2 delay is. Under an `"accrual"` design the study stopped
#' at a calendar date `relative_obs_time` after the start of the line list,
#' and keeps every case whose secondary event fell before it.
#'
#' The advisory checks of [as_epidist_estimates_data()] do not run on the
#' result. They run once on the combined object when several studies are
#' passed to [as_epidist_estimates_data()] in a list.
#'
#' @param data An `epidist_linelist_data` object built from simulated event
#'  times, with the exact `ptime` and `stime` columns kept by
#'  [simulate_dates()] when `keep_times = TRUE`. Every case must use the
#'  same primary window and the same secondary window.
#'
#' @param study A string labelling the study.
#'
#' @param report What the study published. `"moments"` gives a mean and a
#'  standard deviation with the sample size, `"quantiles"` the quantiles at
#'  `probs` with the sample size, `"multivariate"` the mean and standard
#'  deviation with their bootstrap covariance, through
#'  [new_epidist_multivariate()], and `"mean_se"` a mean with its standard
#'  error and no sample size.
#'
#' @param probs The probabilities of the quantiles a `"quantiles"` study
#'  reports.
#'
#' @param cens_adjusted The censoring adjustment code the study used, one of
#'  0 to 4 as described above and in [as_epidist_estimates_data()].
#'
#' @param trunc_adjusted Whether the study corrected for right truncation. If
#'  `FALSE`, the study's truncation is applied to the line list.
#'
#' @param trunc_design How the study stopped collecting, `"cohort"` or
#'  `"accrual"`. Only used when `trunc_adjusted` is `FALSE`.
#'
#' @param relative_obs_time The study's observation time. For a `"cohort"`
#'  design this bounds each delay, and for an `"accrual"` design it is the
#'  length of the collection window from the start of the line list.
#'  Defaults to `Inf`, which is only allowed for a study that adjusted for
#'  right truncation.
#'
#' @param delay_min The smallest measured delay the study counted. Cases
#'  below it are dropped.
#'
#' @param growth_rate The growth rate the study is described by. Passed
#'  through as metadata and not used to select cases, so it should be the
#'  rate the line list was simulated with.
#'
#' @param n The number of cases the study sampled from those it could have
#'  seen. Defaults to `NULL`, meaning all of them.
#'
#' @param max_delay The grid cutoff passed through to
#'  [as_epidist_estimates_data()], or `NULL` for its default.
#'
#' @param ... Further columns, such as covariates, added to every row of the
#'  result.
#'
#' @returns An `epidist_estimates_data` object with one row per reported
#'  summary.
#'
#' @family simulate
#' @importFrom checkmate assert_string assert_flag assert_count assert_number
#' @importFrom checkmate assert_integerish assert_numeric
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export
#' @examples
#' linelist <- simulate_gillespie(seed = 1) |>
#'   simulate_secondary(meanlog = 1.8, sdlog = 0.5) |>
#'   simulate_dates(keep_times = TRUE) |>
#'   as_epidist_linelist_data()
#' simulate_study(
#'   linelist, "naive snapshot",
#'   cens_adjusted = 0, trunc_adjusted = FALSE, relative_obs_time = 15
#' )
simulate_study <- function(
  data,
  study,
  report = c("moments", "quantiles", "multivariate", "mean_se"),
  probs = c(0.25, 0.5, 0.75),
  cens_adjusted = 0,
  trunc_adjusted = FALSE,
  trunc_design = c("cohort", "accrual"),
  relative_obs_time = Inf,
  delay_min = 0,
  growth_rate = 0,
  n = NULL,
  max_delay = NULL,
  ...
) {
  cases <- .study_cases(data)
  assert_string(study)
  report <- match.arg(report)
  trunc_design <- match.arg(trunc_design)
  assert_integerish(cens_adjusted, lower = 0, upper = 4, len = 1)
  cens_adjusted <- as.integer(cens_adjusted)
  assert_flag(trunc_adjusted)
  assert_number(relative_obs_time, lower = 0)
  if (!trunc_adjusted && !is.finite(relative_obs_time)) {
    cli::cli_abort(paste0(
      "A study that did not adjust for right truncation needs a finite ",
      "{.arg relative_obs_time}."
    ))
  }
  assert_number(delay_min, lower = 0, finite = TRUE)
  assert_number(growth_rate, finite = TRUE)
  if (!is.null(n)) {
    assert_count(n, positive = TRUE)
  }
  if (!is.null(max_delay)) {
    assert_number(max_delay, lower = 0, finite = TRUE)
  }
  if (report == "quantiles") {
    assert_numeric(probs, any.missing = FALSE, min.len = 1)
    if (any(probs <= 0 | probs >= 1)) {
      cli::cli_abort(
        "{.arg probs} must be strictly between 0 and 1."
      )
    }
  }

  pwindow <- .study_window(cases$ptime_upr - cases$ptime_lwr, "primary")
  swindow <- .study_window(cases$stime_upr - cases$stime_lwr, "secondary")
  measured <- .study_measured_delay(cases, cens_adjusted, pwindow, swindow)
  seen <- rep(TRUE, nrow(cases))
  if (!trunc_adjusted) {
    seen <- .study_observed(
      cases, measured, cens_adjusted, trunc_design, relative_obs_time,
      swindow
    )
  }
  delays <- measured[seen & measured >= delay_min]
  if (length(delays) == 0) {
    cli::cli_abort(paste0(
      "No case of {.arg data} is observed by study {.val {study}} with its ",
      "observation time and minimum delay."
    ))
  }
  if (!is.null(n) && n < length(delays)) {
    delays <- delays[sample.int(length(delays), n)]
  }

  metadata <- list(
    pwindow = pwindow,
    swindow = swindow,
    relative_obs_time = relative_obs_time,
    trunc_adjusted = trunc_adjusted,
    trunc_design = trunc_design,
    cens_adjusted = cens_adjusted,
    delay_min = delay_min,
    growth_rate = growth_rate,
    max_delay = max_delay,
    ...
  )
  metadata <- metadata[!vapply(metadata, is.null, logical(1))]

  if (report == "multivariate") {
    return(do.call(
      as_epidist_estimates_data,
      c(list(.study_bootstrap(delays), study = study, advise = FALSE), metadata)
    ))
  }
  rows <- .study_rows(delays, report, probs)
  rows <- do.call(mutate, c(list(rows, study = study), metadata))
  return(as_epidist_estimates_data(rows, advise = FALSE))
}

#' The cases of a simulated line list with their exact event times
#'
#' @inheritParams simulate_study
#'
#' @returns A tibble with the exact times checked against their windows.
#'
#' @keywords internal
.study_cases <- function(data) {
  if (!is_epidist_linelist_data(data)) {
    cli::cli_abort(
      "{.arg data} must be an {.cls epidist_linelist_data} object."
    )
  }
  if (!all(c("ptime", "stime") %in% names(data))) {
    cli::cli_abort(paste0(
      "{.arg data} needs the exact event times in {.var ptime} and ",
      "{.var stime} columns. Keep them with ",
      "{.code simulate_dates(keep_times = TRUE)}."
    ))
  }
  cases <- tibble::as_tibble(.drop_epidist_class(data))
  tolerance <- 1e-8
  inside <- cases$ptime >= cases$ptime_lwr - tolerance &
    cases$ptime < cases$ptime_upr + tolerance &
    cases$stime >= cases$stime_lwr - tolerance &
    cases$stime < cases$stime_upr + tolerance
  if (!all(inside)) {
    cli::cli_abort(paste0(
      "The exact event times of {sum(!inside)} case{?s} in {.arg data} fall ",
      "outside their censoring windows. Build the line list with ",
      "{.fn as_epidist_linelist_data} from {.fn simulate_dates} with ",
      "{.code keep_times = TRUE}."
    ))
  }
  return(cases)
}

#' The single censoring window of a simulated study
#'
#' @param width The window widths of every case.
#'
#' @param event Which event the window belongs to, used in the error.
#'
#' @returns The window width.
#'
#' @keywords internal
.study_window <- function(width, event) {
  widths <- unique(width)
  if (length(widths) != 1) {
    cli::cli_abort(paste0(
      "Every case must use the same {event} censoring window, but ",
      "{.arg data} has {length(widths)} widths."
    ))
  }
  return(widths)
}

#' The delay a study measured from each case
#'
#' @param cases The tibble of [.study_cases()].
#'
#' @param cens_adjusted The censoring adjustment code.
#'
#' @param pwindow,swindow The primary and secondary window widths.
#'
#' @returns A numeric vector of measured delays.
#'
#' @keywords internal
.study_measured_delay <- function(cases, cens_adjusted, pwindow, swindow) {
  date_diff <- cases$stime_lwr - cases$ptime_lwr
  return(switch(as.character(cens_adjusted),
    "0" = date_diff,
    "1" = cases$stime - cases$ptime,
    "2" = cases$stime - cases$ptime_lwr,
    "3" = date_diff + swindow / 2,
    "4" = cases$stime - (cases$ptime_lwr + pwindow / 2)
  ))
}

#' Whether a study that did not adjust for right truncation saw each case
#'
#' A cohort study bounds the underlying delay by its observation time, so a
#' study on the discrete grid keeps a case only if the whole window its delay
#' fell in is below the cutoff, and a midpoint primary study keeps a case if
#' its uniform single interval delay is. An accrual study keeps the cases
#' whose secondary event fell before its calendar stop.
#'
#' @inheritParams .study_measured_delay
#'
#' @param measured The measured delays of [.study_measured_delay()].
#'
#' @param trunc_design The truncation design.
#'
#' @param relative_obs_time The study observation time.
#'
#' @returns A logical vector.
#'
#' @keywords internal
.study_observed <- function(
  cases,
  measured,
  cens_adjusted,
  trunc_design,
  relative_obs_time,
  swindow
) {
  if (trunc_design == "accrual") {
    return(cases$stime <= relative_obs_time)
  }
  underlying <- switch(as.character(cens_adjusted),
    "0" = measured + swindow,
    "3" = measured + swindow / 2,
    "4" = cases$stime - cases$ptime_lwr,
    measured
  )
  return(underlying <= relative_obs_time)
}

#' The summary rows a study reports
#'
#' @param delays The measured delays the study summarised.
#'
#' @inheritParams simulate_study
#'
#' @returns A tibble with `type`, `value`, `p`, `n` and `se` columns.
#'
#' @keywords internal
.study_rows <- function(delays, report, probs) {
  size <- as.numeric(length(delays))
  if (report == "moments") {
    return(tibble(
      type = c("mean", "sd"),
      value = c(mean(delays), stats::sd(delays)),
      p = NA_real_,
      n = size,
      se = NA_real_
    ))
  }
  if (report == "mean_se") {
    return(tibble(
      type = "mean",
      value = mean(delays),
      p = NA_real_,
      n = NA_real_,
      se = stats::sd(delays) / sqrt(size)
    ))
  }
  return(tibble(
    type = "quantile",
    value = stats::quantile(delays, probs, names = FALSE),
    p = probs,
    n = size,
    se = NA_real_
  ))
}

#' The bootstrap covariance of a study's mean and standard deviation
#'
#' @param delays The measured delays the study summarised.
#'
#' @param reps The number of bootstrap replicates.
#'
#' @returns An `epidist_multivariate` object holding the mean and standard
#'  deviation of `delays`, their bootstrap covariance and the replicates.
#'
#' @keywords internal
.study_bootstrap <- function(delays, reps = 1000L) {
  draws <- t(vapply(
    seq_len(reps),
    function(i) {
      resample <- sample(delays, replace = TRUE)
      return(c(mean(resample), stats::sd(resample)))
    },
    numeric(2)
  ))
  colnames(draws) <- c("mean", "sd")
  return(new_epidist_multivariate(
    value = c(mean = mean(delays), sd = stats::sd(delays)),
    vcov = stats::cov(draws),
    params = c("mean", "sd"),
    n_draws = reps,
    draws = draws
  ))
}
