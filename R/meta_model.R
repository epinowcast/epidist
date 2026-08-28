#' Convert an object to an `epidist_meta_model` object
#'
#' Creates an `epidist_meta_model` object from individual level data, published
#' summary estimates, or a mix of the two.
#' This enables fitting a single delay distribution to all of the evidence
#' available using [epidist()].
#'
#' The meta model is experimental.
#' Its interface may still change in future releases.
#'
#' Individual level rows use the same likelihood as the marginal model (see
#' [as_epidist_marginal_model()]), imported from the
#' [primarycensored](https://primarycensored.epinowcast.org/) package.
#' Summary rows are instead forward modelled. Given the delay distribution, the
#' model works out what the study's own estimation procedure would have
#' converged to, and fits the reported value to that. Published estimates that
#' did not adjust for right truncation, or that treated interval censored data
#' as continuous, can therefore still contribute unbiased information.
#' That holds only where the metadata describing what each study did is
#' correct. It is usually the analyst's judgement rather than something the
#' study reported, so state it explicitly and vary it in a sensitivity
#' analysis.
#' `vignette("model")` gives the forward model and the sampling likelihoods,
#' and `vignette("meta")` works through a simulated and a real example.
#'
#' At least one of `data` and `estimates` must be supplied. Study level
#' heterogeneity is specified through the `brms` formula in [epidist()], for
#' example `mu ~ 1 + (1 | study)`, rather than through this function.
#' Individual level rows are labelled `"individual"` in the `study` column so
#' that they form their own level of any such term.
#'
#' # What this means in practice
#'
#' Summaries that one study computed from the same delays are correlated, so
#' they are fitted jointly. Two are grouped when they agree on every column of
#' [as_epidist_estimates_data()] other than the summary itself, and a summary
#' supplied with its own `se` is fitted alone. One observation is therefore a
#' group rather than a single reported value, so `log_lik()` and [loo::loo()]
#' report per group, and `loo` only compares fits to the same studies and the
#' same mix of individual and summary rows. See `vignette("faq")`.
#'
#' Three consequences of the sampling likelihoods change what you should do.
#'
#' * The standard errors are plug in quantities that depend on the parameters,
#'   so studies no single distribution can explain may be accommodated by
#'   inflating the implied standard deviation rather than by moving the
#'   location, and sampling can become multimodal. Allow for genuine
#'   differences with a term such as `mu ~ 1 + (1 | study)` rather than relying
#'   on the sampling error alone.
#' * Quantiles read off a fitted distribution rather than the empirical data
#'   have smaller sampling error than assumed here. Supply a reported `se` in
#'   [as_epidist_estimates_data()] for those rows, which also takes them out of
#'   the joint quantile likelihood.
#' * The normal approximations degrade at small study sample sizes, and
#'   summaries of different kinds from one study, such as a mean and a median,
#'   are treated as independent. A study that published draws of its parameters
#'   avoids the second, because [as_epidist_multivariate()] turns them into a
#'   covariance over the summaries that is fitted jointly.
#'
#' Two approximations are worth knowing about before fitting quantiles.
#'
#' * A study that took integer date differences reports quantiles of a discrete
#'   distribution. The model interpolates its grid distribution function
#'   through the mid points of the cells, but the reported value is itself
#'   rounded to that grid, and what is left does not shrink with the study
#'   sample size. It stays under 4% on the mean and 9% on the standard
#'   deviation once the reported quantiles sit twenty five or more cells above
#'   the smallest delay the study counted, and reaches tens of percent on both
#'   when they sit within ten. Refitting the median and
#'   interquartile range of a lognormal delay of mean 5.9 days, on daily
#'   windows with an observation time of 12 days, recovers a delay mean 27%
#'   high and a standard deviation 69% high. The same study's mean and
#'   standard deviation recover the truth, so prefer those where a study
#'   reports them, and check that `swindow` is the resolution it worked at.
#'   [as_epidist_estimates_data()] warns for studies in this range.
#' * The accrual weight applied to a study that stopped collecting at a
#'   calendar date is exact only when `pwindow` and `swindow` are equal. With a
#'   weekly primary and a daily secondary window, a collection window of 28
#'   days and a delay of mean 4.6 days, refitting a reported mean and standard
#'   deviation recovers the standard deviation about 6% high.
#'   `vignette("model")` gives the measurements.
#'
#' Two settings trade accuracy against speed: `max_delay` in
#' [as_epidist_estimates_data()], which sets the grid a study that adjusted for
#' right truncation is summarised on and needs raising for a long tailed delay,
#' and `options(epidist.meta_n_quad = )`, which sets the number of quadrature
#' nodes used where a study is summarised by quadrature instead.
#'
#' @param data An `epidist_linelist_data` or `epidist_aggregate_data` object of
#'  individual level observations, an `epidist_estimates_data` object of
#'  published summary estimates, or `NULL`.
#'
#' @param estimates An `epidist_estimates_data` object of published summary
#'  estimates, or `NULL`.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @family meta_model
#' @export
as_epidist_meta_model <- function(data = NULL, estimates = NULL, ...) {
  # Dispatch explicitly on data so that a summary only call, where data is
  # missing, reaches the NULL method rather than the estimates method.
  UseMethod("as_epidist_meta_model", data)
}

#' The meta model method for `epidist_linelist_data` objects
#'
#' Prepares individual level data exactly as
#' [as_epidist_marginal_model.epidist_linelist_data()] does and then stacks it
#' with any supplied summary estimates.
#'
#' @param data An `epidist_linelist_data` object.
#'
#' @inheritParams as_epidist_meta_model
#' @inheritParams as_epidist_marginal_model.epidist_linelist_data
#'
#' @method as_epidist_meta_model epidist_linelist_data
#'
#' @family meta_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_meta_model()
as_epidist_meta_model.epidist_linelist_data <- function(
  data,
  estimates = NULL,
  obs_time_threshold = 2,
  weight = NULL,
  delay_min = NULL,
  ...
) {
  assert_epidist.epidist_linelist_data(data)
  data <- .prepare_marginal_data(
    data,
    obs_time_threshold = obs_time_threshold,
    weight = weight,
    delay_min = delay_min
  )
  return(.new_meta_model_from_parts(data, estimates))
}

#' The meta model method for `epidist_aggregate_data` objects
#'
#' This method converts aggregate data to a meta model format by passing it to
#' [as_epidist_meta_model.epidist_linelist_data()] with the `n` column used as
#' weights.
#'
#' @param data An `epidist_aggregate_data` object.
#'
#' @inheritParams as_epidist_meta_model.epidist_linelist_data
#'
#' @method as_epidist_meta_model epidist_aggregate_data
#'
#' @family meta_model
#' @autoglobal
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
#'   as_epidist_aggregate_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested",
#'     n = "n"
#'   ) |>
#'   as_epidist_meta_model()
as_epidist_meta_model.epidist_aggregate_data <- function(
  data,
  estimates = NULL,
  obs_time_threshold = 2,
  delay_min = NULL,
  ...
) {
  return(as_epidist_meta_model.epidist_linelist_data(
    data,
    estimates = estimates,
    obs_time_threshold = obs_time_threshold,
    weight = "n",
    delay_min = delay_min,
    ...
  ))
}

#' The meta model method for `epidist_estimates_data` objects
#'
#' Allows summary estimates to be passed as the first argument, so that
#' `as_epidist_meta_model(estimates)` and
#' `as_epidist_meta_model(estimates = estimates)` are equivalent.
#'
#' @param data An `epidist_estimates_data` object.
#'
#' @inheritParams as_epidist_meta_model
#'
#' @method as_epidist_meta_model epidist_estimates_data
#'
#' @family meta_model
#' @export
#' @examples
#' estimates <- as_epidist_estimates_data(
#'   data.frame(
#'     study = c("A", "A"),
#'     type = c("mean", "sd"),
#'     value = c(7.5, 3.6),
#'     n = c(120, 120),
#'     relative_obs_time = c(20, 20),
#'     trunc_adjusted = c(FALSE, FALSE),
#'     cens_adjusted = c(0, 0)
#'   )
#' )
#' as_epidist_meta_model(estimates)
as_epidist_meta_model.epidist_estimates_data <- function(
  data,
  estimates = NULL,
  ...
) {
  if (!is.null(estimates)) {
    cli::cli_abort(paste0(
      "Summary estimates were supplied twice. Pass individual level data as ",
      "{.var data} and summary estimates as {.var estimates}."
    ))
  }
  return(.new_meta_model_from_parts(NULL, data))
}

#' The meta model method for summary estimates only
#'
#' Used when no individual level data is available and only the `estimates`
#' argument is supplied.
#'
#' @param data `NULL`.
#'
#' @inheritParams as_epidist_meta_model
#'
#' @method as_epidist_meta_model NULL
#'
#' @family meta_model
#' @export
#' @examples
#' estimates <- as_epidist_estimates_data(
#'   data.frame(
#'     study = c("A", "A"),
#'     type = c("mean", "sd"),
#'     value = c(7.5, 3.6),
#'     n = c(120, 120),
#'     relative_obs_time = c(20, 20),
#'     trunc_adjusted = c(FALSE, FALSE),
#'     cens_adjusted = c(0, 0)
#'   )
#' )
#' as_epidist_meta_model(estimates = estimates)
as_epidist_meta_model.NULL <- function(data = NULL, estimates = NULL, ...) {
  return(.new_meta_model_from_parts(NULL, estimates))
}

#' Build an `epidist_meta_model` object from its individual and summary parts
#'
#' @param data Individual level data prepared by [.prepare_marginal_data()], or
#'  `NULL`.
#'
#' @param estimates An `epidist_estimates_data` object, or `NULL`.
#'
#' @returns An object of class `epidist_meta_model`.
#'
#' @keywords internal
#' @autoglobal
#' @importFrom dplyr bind_rows
.new_meta_model_from_parts <- function(data, estimates) {
  if (is.null(data) && is.null(estimates)) {
    cli::cli_abort(paste0(
      "The meta model needs at least one of individual level {.var data} and ",
      "summary {.var estimates}."
    ))
  }
  individual_rows <- NULL
  if (!is.null(data)) {
    individual_rows <- .meta_individual_rows(data)
  }
  estimate_rows <- NULL
  members <- .meta_empty_members()
  factors <- numeric(0)
  if (!is.null(estimates)) {
    assert_epidist.epidist_estimates_data(estimates)
    grouped <- .meta_estimate_rows(estimates)
    estimate_rows <- grouped$rows
    members <- grouped$members
    factors <- grouped$chol
  }
  meta_data <- bind_rows(individual_rows, estimate_rows)
  if (hasName(meta_data, "study")) {
    meta_data$study <- as.character(meta_data$study)
    meta_data$study[is.na(meta_data$study)] <- "individual"
  }
  meta_data <- new_epidist_meta_model(meta_data)
  meta_data <- .meta_set_members(meta_data, members, factors)
  assert_epidist(meta_data)
  return(meta_data)
}

#' An empty table of grouped summary members
#'
#' @returns A zero row tibble with the member columns.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_empty_members <- function() {
  return(tibble(
    value = numeric(0),
    count = integer(0),
    type = integer(0),
    p = numeric(0)
  ))
}

#' Attach the grouped summary members to an `epidist_meta_model` object
#'
#' The members of a group row cannot be stored in the row itself because a
#' study may report any number of quantiles, so they are held alongside the
#' data and passed to Stan as flat arrays that the group row indexes into.
#' See [.meta_estimate_rows()].
#'
#' @param data An `epidist_meta_model` object.
#'
#' @param members A tibble of member `value`, `count`, `type` and `p` columns.
#'
#' @param chol A flat numeric vector of Cholesky factor entries, in column
#'  major order, for the groups covered by a covariance matrix.
#'
#' @returns The input with the members attached.
#'
#' @keywords internal
.meta_set_members <- function(data, members, chol = numeric(0)) {
  attr(data, "meta_members") <- members
  attr(data, "meta_chol") <- chol
  return(data)
}

#' The grouped summary members of an `epidist_meta_model` object
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns A tibble of member `value`, `count`, `type` and `p` columns.
#'
#' @keywords internal
.meta_members <- function(data) {
  members <- attr(data, "meta_members")
  if (is.null(members)) {
    return(.meta_empty_members())
  }
  return(members)
}

#' The flat Cholesky factors of an `epidist_meta_model` object
#'
#' A group covered by a covariance matrix has the Cholesky factor of that
#' matrix stored here, in column major order, and its row indexes into this
#' vector with `chol_start`. Factoring once here rather
#' than inside the likelihood keeps the matrix out of every gradient
#' evaluation.
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns A numeric vector of Cholesky factor entries.
#'
#' @keywords internal
.meta_chol <- function(data) {
  factors <- attr(data, "meta_chol")
  if (is.null(factors)) {
    return(numeric(0))
  }
  return(factors)
}

#' Build the individual level rows of an `epidist_meta_model` object
#'
#' @param data Individual level data prepared by [.prepare_marginal_data()].
#'
#' @returns A tibble of individual level rows using the meta model slots.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_individual_rows <- function(data) {
  # Work on a plain tibble so that dropping the consumed columns does not
  # revalidate, and warn about, the input class.
  data <- tibble::as_tibble(unclass(data))
  rows <- tibble(
    delay_lwr = as.integer(data$delay_lwr),
    n = data$n,
    obs_type = 1L,
    study_n = 0L,
    trunc_adjusted = 0L,
    trunc_design = 0L,
    cens_adjusted = 0L,
    group_start = 1L,
    group_len = 0L,
    chol_start = 1L,
    relative_obs_time = as.numeric(data$relative_obs_time),
    pwindow = as.numeric(data$pwindow),
    swindow = as.numeric(data$swindow),
    delay_upr = as.numeric(data$delay_upr),
    delay_min = as.numeric(data$delay_min),
    report_se = 0,
    quantile_p = 0,
    growth_rate = 0
  )
  extra <- data[setdiff(names(data), names(rows))]
  return(bind_cols(rows, extra))
}

#' Assign summary estimates to joint likelihood groups
#'
#' Summaries reported by the same study are fitted jointly, so they must be
#' collected into groups before the model rows are built. Two summaries share a
#' group only when they agree on every column other than the summary itself,
#' that is on the study, its metadata, and any covariates the user supplied.
#' This keeps the grouping correct for any `brms` formula, because a linear
#' predictor built from those columns cannot vary within a group.
#'
#' Means and standard deviations group together, and quantiles group together,
#' but the two are not mixed because their joint likelihoods differ. A study
#' reporting more than one mean, or more than one standard deviation, with
#' otherwise identical metadata has the repeats split into further groups. A
#' summary with its own reported standard error is left on its own, because
#' that standard error replaces the sampling uncertainty the joint likelihood
#' would derive.
#'
#' The summaries covered by one covariance matrix are one group, whatever their
#' types, because the matrix is what ties them together. They are identified by
#' their shared `mvn_id`, so one study may contribute more than one such group.
#'
#' @param estimates An `epidist_estimates_data` object.
#'
#' @param mvn A logical vector marking the rows covered by a reported
#'  covariance matrix.
#'
#' @returns An integer vector of group identifiers, one per row.
#'
#' @keywords internal
.meta_assign_groups <- function(estimates, mvn = NULL) {
  separator <- rawToChar(as.raw(31L))
  key_cols <- setdiff(names(estimates), c("type", "value", "se", "p"))
  parts <- lapply(estimates[key_cols], as.character)
  kind <- ifelse(estimates$type == "quantile", "quantile", "moment")
  key <- do.call(paste, c(unname(parts), list(kind), list(sep = separator)))
  # A study reporting two means, or two standard deviations, with otherwise
  # identical metadata has the repeats split into further groups. Several
  # quantiles are instead members of one set.
  moment <- kind == "moment"
  repeats <- rep(1L, length(key))
  repeats[moment] <- stats::ave(
    seq_len(sum(moment)),
    paste(key[moment], estimates$type[moment]),
    FUN = seq_along
  )
  key <- paste(key, repeats, sep = separator)
  solo <- !is.na(estimates$se)
  key[solo] <- paste("solo", which(solo), sep = separator)
  if (!is.null(mvn) && any(mvn)) {
    key[mvn] <- paste("mvn", estimates$mvn_id[mvn], sep = separator)
  }
  return(match(key, unique(key)))
}

#' Build the summary estimate rows of an `epidist_meta_model` object
#'
#' One row is built per group from [.meta_assign_groups()]. A group of one is a
#' single reported summary, and a larger group is fitted with the joint
#' likelihood of its members. The members of a group are held in flat arrays
#' that the row indexes with `group_start` and `group_len`, because a study may
#' report any number of quantiles and a row has a fixed number of slots. Row
#' order is irrelevant to the index, so aggregating or reordering rows later
#' cannot break it.
#'
#' @param estimates An `epidist_estimates_data` object.
#'
#' @returns A list with a tibble of summary `rows` using the meta model slots,
#'  a tibble of their `members`, and the flat `chol` vector of Cholesky
#'  factors for the groups covered by a covariance matrix.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_estimate_rows <- function(estimates) {
  supplied <- .estimates_vcov(estimates)
  mvn <- .estimates_vcov_rows(estimates)
  estimates <- tibble::as_tibble(unclass(estimates))
  group <- .meta_assign_groups(estimates, mvn)
  index <- split(seq_len(nrow(estimates)), group)
  index <- index[order(vapply(index, min, numeric(1)))]
  built <- lapply(index, function(rows) {
    key <- estimates$mvn_id[rows[1]]
    return(.meta_group_row(
      estimates[rows, , drop = FALSE],
      if (all(mvn[rows])) supplied[[key]] else NULL
    ))
  })
  members <- lapply(built, "[[", "members")
  factors <- lapply(built, "[[", "chol")
  rows <- bind_rows(lapply(built, "[[", "row"))
  sizes <- vapply(members, nrow, numeric(1))
  entries <- lengths(factors)
  rows$group_start <- as.integer(cumsum(c(1, sizes))[seq_along(sizes)])
  rows$group_len <- as.integer(sizes)
  rows$chol_start <- as.integer(cumsum(c(1, entries))[seq_along(entries)])
  return(list(
    rows = rows,
    members = bind_rows(members),
    chol = as.numeric(unlist(factors, use.names = FALSE))
  ))
}

#' Build a single meta model row from one group of summary estimates
#'
#' The `group_start` slot is filled in by [.meta_estimate_rows()] once every
#' group has been built.
#'
#' @param estimates The rows of an `epidist_estimates_data` object making up
#'  one group.
#'
#' @param vcov The covariance matrix over the group's summaries, or `NULL`
#'  where the study reported standard errors or a sample size instead.
#'
#' @returns A list with a one row tibble `row`, a tibble of its `members`, and
#'  the flat `chol` entries of its covariance matrix.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_group_row <- function(estimates, vcov = NULL) {
  estimates <- .meta_order_group(estimates, vcov)
  study_n <- as.integer(ifelse(is.na(estimates$n[1]), 0L, estimates$n[1]))
  quantiles <- estimates$type[1] == "quantile"
  joint <- nrow(estimates) > 1 || (quantiles && is.na(estimates$se[1]))
  members <- .meta_empty_members()
  factor_entries <- numeric(0)
  obs_type <- .meta_obs_type(estimates$type[1])
  if (!is.null(vcov)) {
    obs_type <- 7L
    members <- .meta_member_table(estimates, rep(0L, nrow(estimates)))
    # The lower factor flattened column major, which is the order Stan's
    # to_matrix reads and multi_normal_cholesky_lpdf expects.
    factor_entries <- as.numeric(t(chol(vcov)))
  } else if (joint && quantiles) {
    obs_type <- 6L
    members <- .meta_member_table(
      estimates, .meta_quantile_counts(estimates$p, study_n)
    )
  } else if (joint) {
    obs_type <- 5L
    members <- .meta_member_table(estimates, rep(0L, nrow(estimates)))
  }
  group <- tibble(
    delay_lwr = 0L,
    n = 1,
    obs_type = obs_type,
    study_n = study_n,
    trunc_adjusted = as.integer(estimates$trunc_adjusted[1]),
    trunc_design = .meta_trunc_design(estimates$trunc_design[1]),
    cens_adjusted = as.integer(estimates$cens_adjusted[1]),
    group_start = 1L,
    group_len = nrow(members),
    chol_start = 1L,
    relative_obs_time = as.numeric(.estimates_grid_cutoff(estimates)[1]),
    pwindow = as.numeric(estimates$pwindow[1]),
    swindow = as.numeric(estimates$swindow[1]),
    delay_upr = as.numeric(estimates$value[1]),
    delay_min = as.numeric(estimates$delay_min[1]),
    report_se = ifelse(is.na(estimates$se[1]), 0, estimates$se[1]),
    quantile_p = ifelse(is.na(estimates$p[1]), 0, estimates$p[1]),
    growth_rate = as.numeric(estimates$growth_rate[1])
  )
  consumed <- setdiff(.estimates_required_cols(), "study")
  extra <- estimates[setdiff(names(estimates), c(names(group), consumed))]
  return(list(
    row = bind_cols(group, extra[1, , drop = FALSE]),
    members = members,
    chol = factor_entries
  ))
}

#' Build the member table of one joint likelihood group
#'
#' The member type and probability are carried alongside the reported value
#' because a group covered by a covariance matrix may mix means, standard
#' deviations and quantiles, so the likelihood needs to know which implied
#' summary each member is.
#'
#' @param estimates The rows of an `epidist_estimates_data` object making up
#'  one group, already ordered.
#'
#' @param count The cumulative counts the multinomial quantile likelihood
#'  uses, or zeros for a group that does not use it.
#'
#' @returns A tibble of member `value`, `count`, `type` and `p` columns.
#'
#' @keywords internal
#' @importFrom tibble tibble
.meta_member_table <- function(estimates, count) {
  return(tibble(
    value = as.numeric(estimates$value),
    count = as.integer(count),
    type = as.integer(match(estimates$type, .estimates_types())),
    p = ifelse(is.na(estimates$p), 0, as.numeric(estimates$p))
  ))
}

#' Order the summaries within a joint likelihood group
#'
#' A mean and standard deviation pair is stored with the mean first so that the
#' bivariate normal knows which member is which. A set of quantiles is stored
#' in increasing probability, which must also be increasing in the reported
#' value for the cells of the multinomial to be a partition of the delay axis.
#'
#' A group covered by a covariance matrix keeps the order its rows were given
#' in, because that is the order the matrix is indexed by.
#'
#' @param estimates The rows of an `epidist_estimates_data` object making up
#'  one group.
#'
#' @param vcov The covariance matrix over the group's summaries, or `NULL`.
#'
#' @returns The input, reordered.
#'
#' @keywords internal
.meta_order_group <- function(estimates, vcov = NULL) {
  if (nrow(estimates) == 1 || !is.null(vcov)) {
    return(estimates)
  }
  if (estimates$type[1] == "quantile") {
    estimates <- estimates[order(estimates$p), , drop = FALSE]
    if (any(diff(estimates$p) <= 0)) {
      cli::cli_abort(paste0(
        "{.val {estimates$study[1]}} reports two quantiles at the same ",
        "probability {.var p} with the same study metadata, so they cannot ",
        "both describe its delays."
      ))
    }
    if (any(diff(estimates$value) <= 0)) {
      cli::cli_abort(paste0(
        "The quantiles reported by {.val {estimates$study[1]}} must increase ",
        "with their probability {.var p}."
      ))
    }
    return(estimates)
  }
  member_order <- order(match(estimates$type, c("mean", "sd")))
  return(estimates[member_order, , drop = FALSE])
}

#' Map truncation designs to their slot codes
#'
#' @param design A character vector of truncation designs.
#'
#' @returns An integer vector, 0 for a cohort design and 1 for an accrual
#'  design.
#'
#' @keywords internal
.meta_trunc_design <- function(design) {
  return(match(design, .estimates_trunc_designs()) - 1L)
}

#' Map summary types to their observation type codes
#'
#' @param type A character vector of summary types.
#'
#' @returns An integer vector of observation type codes.
#'
#' @keywords internal
.meta_obs_type <- function(type) {
  return(match(type, .estimates_types()) + 1L)
}

#' Class constructor for `epidist_meta_model` objects
#'
#' @param data A data.frame to convert
#'
#' @returns An object of class `epidist_meta_model`
#'
#' @family meta_model
#' @export
new_epidist_meta_model <- function(data) {
  class(data) <- c("epidist_meta_model", class(data))
  return(data)
}

#' Check if data has the `epidist_meta_model` class
#'
#' @param data A `data.frame` to check.
#'
#' @family meta_model
#' @export
is_epidist_meta_model <- function(data) {
  return(inherits(data, "epidist_meta_model"))
}

#' Assert validity of `epidist_meta_model` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments
#'
#' @method assert_epidist epidist_meta_model
#'
#' @family meta_model
#' @export
assert_epidist.epidist_meta_model <- function(data, ...) {
  assert_data_frame(data)
  assert_names(names(data), must.include = .meta_required_cols())
  assert_subset(data$obs_type, 1:7, .var.name = "obs_type")
  assert_subset(data$trunc_design, 0:1, .var.name = "trunc_design")
  assert_subset(data$cens_adjusted, 0:4, .var.name = "cens_adjusted")
  assert_integerish(data$delay_lwr)
  assert_numeric(data$n, lower = 0)
  assert_numeric(data$pwindow, lower = 0)
  assert_numeric(data$swindow, lower = 0)
  assert_numeric(data$relative_obs_time, lower = 0)
  assert_numeric(data$delay_upr, lower = 0)
  assert_numeric(data$delay_min, lower = 0, any.missing = FALSE)
  assert_numeric(data$report_se, lower = 0)
  assert_numeric(data$quantile_p, lower = 0, upper = 1)
  assert_numeric(data$growth_rate, finite = TRUE)

  individual <- data[data$obs_type == 1L, , drop = FALSE]
  if (nrow(individual) > 0) {
    if (!all(
      abs(individual$delay_upr - (individual$delay_lwr + individual$swindow)) <
        1e-10
    )) {
      cli::cli_abort(paste0(
        "{.var delay_upr} must equal {.var delay_lwr} + {.var swindow} for ",
        "individual level rows."
      ))
    }
    if (!all(individual$relative_obs_time >= individual$delay_upr)) {
      cli::cli_abort(paste0(
        "{.var relative_obs_time} must be greater than or equal to ",
        "{.var delay_upr} for individual level rows."
      ))
    }
    if (!all(individual$delay_lwr >= individual$delay_min)) {
      cli::cli_abort(paste0(
        "{.var delay_lwr} must be greater than or equal to ",
        "{.var delay_min} for individual level rows."
      ))
    }
  }

  if (!all(data$relative_obs_time > data$delay_min)) {
    cli::cli_abort(
      "{.var delay_min} must be below {.var relative_obs_time}."
    )
  }

  summaries <- data[data$obs_type != 1L, , drop = FALSE]
  if (nrow(summaries) > 0) {
    covariance <- summaries$obs_type == 7L
    if (any(summaries$study_n[!covariance] < 2 &
      summaries$report_se[!covariance] <= 0)) {
      cli::cli_abort(paste0(
        "Summary rows need either a {.var study_n} of at least 2 or a ",
        "reported {.var report_se}."
      ))
    }
    if (!all(is.finite(summaries$relative_obs_time))) {
      cli::cli_abort(paste0(
        "Summary rows need a finite grid cutoff in ",
        "{.var relative_obs_time}."
      ))
    }
    if (any(summaries$relative_obs_time < summaries$swindow)) {
      cli::cli_abort(
        "The grid cutoff for summary rows must be at least {.var swindow}."
      )
    }
    quantiles <- summaries[summaries$obs_type %in% c(4L, 6L), , drop = FALSE]
    if (any(quantiles$quantile_p <= 0 | quantiles$quantile_p >= 1)) {
      cli::cli_abort(paste0(
        "Quantile rows need a probability strictly between 0 and 1 in ",
        "{.var quantile_p}."
      ))
    }
  }

  .assert_meta_groups(data)

  return(invisible(NULL))
}

#' Assert that the group rows of an `epidist_meta_model` object are consistent
#'
#' A group row stands for several summaries reported by one study and indexes
#' them in the flat member arrays. This checks that every index lands inside
#' those arrays, that each kind of group has the members its likelihood needs,
#' and that a set of quantiles is a partition of the delay axis.
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
.assert_meta_groups <- function(data) {
  members <- .meta_members(data)
  factors <- .meta_chol(data)
  assert_integerish(data$group_start, lower = 1, any.missing = FALSE)
  assert_integerish(data$group_len, lower = 0, any.missing = FALSE)
  assert_integerish(data$chol_start, lower = 1, any.missing = FALSE)
  if (any(data$group_start + data$group_len - 1L > nrow(members))) {
    cli::cli_abort(paste0(
      "Every {.var group_start} and {.var group_len} must index within the ",
      "grouped summary members of the model."
    ))
  }
  covariance <- data$obs_type == 7L
  if (any(covariance)) {
    if (any(data$group_len[covariance] < 1L)) {
      cli::cli_abort(paste0(
        "A row fitted with a reported covariance matrix must have at least ",
        "one grouped summary member."
      ))
    }
    entries <- data$chol_start[covariance] + data$group_len[covariance]^2 - 1L
    if (any(entries > length(factors))) {
      cli::cli_abort(paste0(
        "Every {.var chol_start} must index a full Cholesky factor within ",
        "the factors of the model."
      ))
    }
  }
  if (any(data$obs_type == 5L & data$group_len != 2L)) {
    cli::cli_abort(paste0(
      "A joint mean and standard deviation row must have exactly two grouped ",
      "summary members."
    ))
  }
  if (any(data$obs_type == 6L & data$group_len < 1L)) {
    cli::cli_abort(
      "A joint quantile row must have at least one grouped summary member."
    )
  }
  for (i in which(data$obs_type == 6L)) {
    member <- seq_len(data$group_len[i]) + data$group_start[i] - 1L
    if (any(diff(members$value[member]) <= 0)) {
      cli::cli_abort(
        "The quantiles of a joint quantile row must be strictly increasing."
      )
    }
    count <- members$count[member]
    if (any(diff(c(0L, count)) < 0) || any(count > data$study_n[i])) {
      cli::cli_abort(paste0(
        "The cumulative counts of a joint quantile row must be non ",
        "decreasing and no larger than the study sample size."
      ))
    }
  }
  return(invisible(NULL))
}

#' Create the model-specific component of an `epidist` custom family
#'
#' @inheritParams epidist_family_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_family_model epidist_meta_model
#'
#' @family meta_model
#' @export
epidist_family_model.epidist_meta_model <- function(
  data,
  family,
  ...
) {
  custom_family <- brms::custom_family(
    paste0("meta_", family$family),
    dpars = family$dpars,
    links = c(family$link, family$other_links),
    lb = c(
      as.numeric(family$ybounds[1]),
      as.numeric(lapply(family$other_bounds, "[[", "lb"))
    ),
    ub = c(
      as.numeric(family$ybounds[2]),
      as.numeric(lapply(family$other_bounds, "[[", "ub"))
    ),
    type = "int",
    vars = c(
      paste0("vint", 1:8, "[n]"),
      paste0("vreal", 1:8, "[n]"),
      "meta_group_value",
      "meta_group_count",
      "meta_group_type",
      "meta_group_p",
      "meta_group_chol",
      "primary_params"
    ),
    loop = TRUE,
    log_lik = epidist_gen_meta_log_lik(family),
    posterior_predict = epidist_gen_meta_predict(family),
    posterior_epred = epidist_gen_posterior_epred(family)
  )
  return(custom_family)
}

#' Define the model-specific component of an `epidist` custom formula for the
#' meta model
#'
#' @inheritParams epidist_formula_model
#'
#' @param ... Additional arguments passed to method.
#'
#' @method epidist_formula_model epidist_meta_model
#'
#' @family meta_model
#' @export
epidist_formula_model.epidist_meta_model <- function(
  data,
  formula,
  ...
) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula,
    delay_lwr |
      weights(n) +
        vint(
          obs_type,
          study_n,
          trunc_adjusted,
          cens_adjusted,
          trunc_design,
          group_start,
          group_len,
          chol_start
        ) +
        vreal(
          relative_obs_time,
          pwindow,
          swindow,
          delay_upr,
          delay_min,
          report_se,
          quantile_p,
          growth_rate
        ) ~
      .
  )
  return(formula)
}

#' Transform data for the meta model
#'
#' Individual level rows are summarised by counting unique combinations of the
#' meta model columns and any variables in the model formula, exactly as
#' [epidist_transform_data_model.epidist_marginal_model()] does. Summary rows
#' are passed through unchanged because each one is a distinct reported
#' quantity.
#'
#' @param data The data to transform
#'
#' @param family The epidist family object specifying the distribution
#'
#' @param formula The model formula
#'
#' @param ... Additional arguments passed to methods
#'
#' @method epidist_transform_data_model epidist_meta_model
#' @family meta_model
#' @autoglobal
#' @export
epidist_transform_data_model.epidist_meta_model <- function(
  data,
  family,
  formula,
  ...
) {
  required_cols <- .meta_required_cols()
  extra_cols <- intersect("study", names(data))
  plain_data <- tibble::as_tibble(unclass(data))

  individual <- plain_data[plain_data$obs_type == 1L, , drop = FALSE]
  summaries <- plain_data[plain_data$obs_type != 1L, , drop = FALSE]

  if (nrow(individual) > 0) {
    individual <- .summarise_n_by_formula(
      individual,
      by = c(required_cols, extra_cols),
      formula = formula
    )
  }

  trans_data <- new_epidist_meta_model(bind_rows(individual, summaries))
  trans_data <- .meta_set_members(
    trans_data, .meta_members(data), .meta_chol(data)
  )

  .inform_data_summarised(data, trans_data, c(required_cols, extra_cols))

  return(trans_data)
}

#' @method epidist_stancode epidist_meta_model
#' @importFrom brms stanvar
#' @family meta_model
#' @autoglobal
#' @export
epidist_stancode.epidist_meta_model <- function(
  data,
  family = epidist_family(data),
  formula = epidist_formula(data),
  ...
) {
  assert_epidist.epidist_meta_model(data)

  stanvars_version <- .version_stanvar()

  # n_quad_default keeps the Stan quadrature resolution equal to the one R
  # uses.
  stanvars_functions <- .family_functions_stanvar(
    file.path("meta_model", "functions.stan"), family, "meta_",
    extra = c(n_quad_default = as.character(.meta_n_quad()))
  )

  stanvars_parameters <- brms::stanvar(
    block = "parameters",
    scode = "array[0] real primary_params;"
  )

  pcd_stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = primarycensored::pcd_load_stan_functions(
      c(
        "primarycensored_lpmf",
        "primarycensored_lcdf",
        "primarycensored_ode",
        "dist_lcdf",
        "primary_lpdf"
      ),
      dependencies = TRUE
    )
  )

  stanvars_all <- stanvars_version +
    stanvars_functions +
    pcd_stanvars_functions +
    stanvars_parameters +
    .meta_group_stanvars(data)

  return(stanvars_all)
}

#' Pass the grouped summary members of a meta model to Stan
#'
#' A row that stands for several summaries reported by one study cannot carry
#' them in its own slots, because a study may report any number of quantiles.
#' They are passed instead as flat arrays that the row indexes into with its
#' `group_start` and `group_len` slots. See [.meta_estimate_rows()].
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns A `brms` `stanvars` object holding the flat member arrays.
#'
#' @keywords internal
#' @importFrom brms stanvar
.meta_group_stanvars <- function(data) {
  members <- .meta_members(data)
  factors <- .meta_chol(data)
  size <- brms::stanvar(
    x = nrow(members),
    name = "N_meta_group",
    scode = "int<lower=0> N_meta_group;",
    block = "data"
  )
  value <- brms::stanvar(
    x = as.array(as.numeric(members$value)),
    name = "meta_group_value",
    scode = "vector[N_meta_group] meta_group_value;",
    block = "data"
  )
  count <- brms::stanvar(
    x = as.array(as.integer(members$count)),
    name = "meta_group_count",
    scode = "array[N_meta_group] int meta_group_count;",
    block = "data"
  )
  type <- brms::stanvar(
    x = as.array(as.integer(members$type)),
    name = "meta_group_type",
    scode = "array[N_meta_group] int meta_group_type;",
    block = "data"
  )
  prob <- brms::stanvar(
    x = as.array(as.numeric(members$p)),
    name = "meta_group_p",
    scode = "vector[N_meta_group] meta_group_p;",
    block = "data"
  )
  chol_size <- brms::stanvar(
    x = length(factors),
    name = "N_meta_chol",
    scode = "int<lower=0> N_meta_chol;",
    block = "data"
  )
  chol_entries <- brms::stanvar(
    x = as.array(as.numeric(factors)),
    name = "meta_group_chol",
    scode = "vector[N_meta_chol] meta_group_chol;",
    block = "data"
  )
  return(size + value + count + type + prob + chol_size + chol_entries)
}

.meta_required_cols <- function() {
  return(c(
    "delay_lwr",
    "n",
    "obs_type",
    "study_n",
    "trunc_adjusted",
    "trunc_design",
    "cens_adjusted",
    "group_start",
    "group_len",
    "chol_start",
    "relative_obs_time",
    "pwindow",
    "swindow",
    "delay_upr",
    "delay_min",
    "report_se",
    "quantile_p",
    "growth_rate"
  ))
}
