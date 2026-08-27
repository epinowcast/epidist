#' Report draws of summary quantities as a published estimate
#'
#' Turns draws of the quantities a study reports into the summary vector and
#' covariance matrix that [as_epidist_estimates_data()] takes. Rows are draws
#' and columns are reported quantities, so anything able to produce draws of
#' what a study would report can be used.
#'
#' Posterior draws from a model already fitted to the delays are the common
#' case, and [predict_delay_parameters()] returns them for a fit made with this
#' package. Its `mean` and `sd` columns are draws of exactly the quantities a
#' study reports. It returns one row per draw for each row of the data the fit
#' used, so take a single `index` before passing them in, or supply the
#' `newdata` the summary should describe. Resampling a set of delays is another
#' way to produce draws, and [delays_to_multivariate()] wraps that.
#'
#' The reported vector is the mean of each column and the covariance is the
#' covariance of the draws, so the correlation between the reported quantities
#' is kept. Reporting a standard error for each summary on its own throws that
#' correlation away, and a meta analysis of the published numbers then loses
#' more than it needs to.
#'
#' The number of delays the study summarised is not taken from the draws,
#' because draws carry no such count. Supply it, and any other study metadata,
#' through `...`. The reporting table of Charniga et al. (2024) covers most of
#' what is needed, and passing those fields through `...` puts them straight
#' onto the returned rows. A study reporting a covariance matrix needs no sample
#' size, because the matrix already gives the uncertainty of every summary.
#'
#' @param draws A matrix or data frame of draws, with one row per draw and one
#'  column per reported quantity. More draws than columns are needed, or the
#'  covariance matrix is singular.
#'
#' @param study A string naming the study the draws come from.
#'
#' @param type A character vector giving what each column of `draws` is, one of
#'  `"mean"`, `"sd"` or `"quantile"` per column. Defaults to `NULL`, which uses
#'  the column names of `draws` where all of them are already supported types.
#'
#' @param p A numeric vector of probabilities for the columns with a `type` of
#'  `"quantile"`. Either one entry per quantile column, or one per column with
#'  the entries for other types ignored. Required when any column is a
#'  quantile.
#'
#' @param value A numeric vector of reported values, one per column of `draws`.
#'  Defaults to `NULL`, which reports the mean of each column. Supply it where
#'  the study would publish a point estimate computed some other way, such as
#'  the summary of the delays a resampling procedure started from.
#'
#' @param ... Further columns to attach to every returned row, such as the
#'  sample size `n` and the study metadata documented in
#'  [as_epidist_estimates_data()].
#'
#' @returns A list with a `data` frame of reported summaries and a `vcov` list
#'  holding the covariance matrix, both ready to pass to
#'  [as_epidist_estimates_data()].
#'
#' @family estimates_data
#' @importFrom tibble tibble
#' @importFrom checkmate assert_numeric assert_string
#' @export
#' @examples
#' set.seed(1)
#' draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
#' reported <- draws_to_multivariate(draws, study = "site A", n = 200)
#' as_epidist_estimates_data(reported$data, vcov = reported$vcov)
draws_to_multivariate <- function(
  draws,
  study,
  type = NULL,
  p = NULL,
  value = NULL,
  ...
) {
  assert_string(study)
  draws <- .as_estimates_draws(draws)
  type <- .estimates_draw_types(type, colnames(draws), ncol(draws))
  p <- .estimates_draw_p(p, type)
  if (nrow(draws) <= ncol(draws)) {
    cli::cli_abort(paste0(
      "{.var draws} holds {nrow(draws)} draws of {ncol(draws)} quantit",
      "{?y/ies}, so their covariance matrix is singular. More draws than ",
      "reported quantities are needed."
    ))
  }
  if (is.null(value)) {
    value <- colMeans(draws)
  }
  assert_numeric(
    value,
    len = ncol(draws), any.missing = FALSE, finite = TRUE,
    .var.name = "value"
  )
  covariance <- stats::cov(draws)
  dimnames(covariance) <- NULL
  if (inherits(try(chol(covariance), silent = TRUE), "try-error")) {
    cli::cli_abort(paste0(
      "The draws imply a covariance matrix that is not positive definite. ",
      "This happens when a column of {.var draws} is constant, or when two ",
      "of them are exact copies of each other."
    ))
  }
  return(list(
    data = tibble(
      study = study, type = type, value = unname(value), p = p, ...
    ),
    vcov = stats::setNames(list(covariance), study)
  ))
}

#' Report a set of delays as a published estimate
#'
#' Summarises individual level delays and resamples them to estimate the
#' covariance between the summaries, giving the format
#' [draws_to_multivariate()] returns. This is what a site holding a line list it
#' cannot release would run and publish instead of the line list.
#'
#' The reported values are the summaries of the delays themselves rather than
#' the means of the resampled draws, so they are what the study would report.
#' Only the covariance comes from the resampling.
#'
#' @param delays A numeric vector of observed delays.
#'
#' @param study A string naming the study the delays come from.
#'
#' @param moments Which moments to report, any of `"mean"` and `"sd"`.
#'
#' @param probs A numeric vector of probabilities to report quantiles at.
#'
#' @param n_bootstrap The number of resamples used to estimate the covariance.
#'  Must exceed the number of summaries reported.
#'
#' @inheritParams draws_to_multivariate
#'
#' @inherit draws_to_multivariate return
#'
#' @family estimates_data
#' @importFrom checkmate assert_integerish
#' @export
#' @examples
#' set.seed(1)
#' reported <- delays_to_multivariate(
#'   rlnorm(200, 1.6, 0.5),
#'   study = "site A",
#'   probs = c(0.25, 0.5, 0.75),
#'   cens_adjusted = 1
#' )
#' as_epidist_estimates_data(reported$data, vcov = reported$vcov)
delays_to_multivariate <- function(
  delays,
  study,
  moments = c("mean", "sd"),
  probs = numeric(0),
  n_bootstrap = 1000,
  ...
) {
  assert_numeric(delays, min.len = 2, any.missing = FALSE, finite = TRUE)
  assert_subset(moments, c("mean", "sd"), .var.name = "moments")
  assert_numeric(probs, lower = 0, upper = 1, any.missing = FALSE)
  assert_integerish(n_bootstrap, lower = 2, len = 1, any.missing = FALSE)
  moments <- intersect(c("mean", "sd"), moments)
  n_summary <- length(moments) + length(probs)
  if (n_summary == 0) {
    cli::cli_abort(
      "Report at least one of {.var moments} and {.var probs}."
    )
  }
  if (n_bootstrap <= n_summary) {
    cli::cli_abort(paste0(
      "{.var n_bootstrap} must exceed the {n_summary} summar{?y/ies} ",
      "reported, or the covariance matrix is singular."
    ))
  }
  summarise <- function(x) {
    return(c(
      if ("mean" %in% moments) mean(x),
      if ("sd" %in% moments) stats::sd(x),
      stats::quantile(x, probs, names = FALSE)
    ))
  }
  draws <- t(vapply(
    seq_len(n_bootstrap),
    function(i) {
      return(summarise(sample(delays, replace = TRUE)))
    },
    numeric(n_summary)
  ))
  dots <- list(...)
  if (!hasName(dots, "n")) {
    dots$n <- length(delays)
  }
  return(do.call(
    draws_to_multivariate,
    c(
      list(
        draws = draws,
        study = study,
        type = c(moments, rep("quantile", length(probs))),
        p = probs,
        value = summarise(delays)
      ),
      dots
    )
  ))
}

#' Report a fitted delay distribution as a published estimate
#'
#' Studies often publish the parameters of a distribution they fitted rather
#' than the summaries of the delays themselves. This converts those parameters,
#' and their uncertainty, into the summaries that the fitted distribution
#' implies, in the format [as_epidist_estimates_data()] takes.
#'
#' # What the reported parameters are taken to mean
#'
#' The reported parameters describe the distribution the study's own estimation
#' procedure converged to. Where the procedure was correct, that is the delay
#' distribution itself. Where it was not, it is the biased distribution the
#' procedure targeted, which is exactly what the meta model forward models from
#' the study metadata. Converting to summaries therefore covers both cases with
#' one route, and the study metadata documented in
#' [as_epidist_estimates_data()] is supplied in the same way as for any other
#' summary.
#'
#' Because the returned rows are summaries, the family the study fitted need
#' not match the family being fitted to it. A study reporting a gamma can be
#' used in a lognormal meta model. Parameters are never compared across
#' families, which would be meaningless. For a two parameter family the map
#' from parameters to a mean and a standard deviation is one to one, so a study
#' that fitted correctly loses nothing by being converted.
#'
#' What this does assume is that summaries of the study's reported distribution
#' are the summaries of what its procedure targeted. That holds exactly when
#' the study reported a distribution of the same shape as its estimand, and
#' approximately otherwise, for example where a study fitted a continuous
#' distribution to integer date differences.
#'
#' # The range the summaries are taken over
#'
#' A study that did not correct for right truncation fitted a distribution to
#' delays that stop at its observation time, but the families studies fit have
#' a tail running past that point. Taking the reported distribution's summaries
#' over its whole support would then charge it with spread its data never had,
#' which is worst for a standard deviation and can reach tens of percent for a
#' short observation time.
#'
#' Summaries are therefore taken over the range of delays the study could have
#' seen, conditioning the reported distribution on falling between `delay_min`
#' and `relative_obs_time`. This is the same range the meta model computes its
#' implied summaries over, so the two sides are the same functional of the two
#' distributions. Both fields are read from the metadata passed through `...`,
#' and a study that adjusted for right truncation is left unconditioned above.
#' A reported quantile at probability `p` is the value the conditioned
#' distribution puts `p` below, so it too stays inside the observed range.
#'
#' Where the study fitted a family that cannot represent its own estimand,
#' quantiles inside the body of the distribution are more reliable than a
#' standard deviation, which depends on a tail the study never saw.
#'
#' # Uncertainty
#'
#' Supply the uncertainty the study reported on its parameters through `vcov`,
#' or through `se` where only per parameter standard errors are given. It is
#' carried onto the summary scale by the delta method, using a numerical
#' Jacobian of the map from parameters to summaries. This is the uncertainty of
#' a reported fitted distribution, and it is usually narrower than the sampling
#' uncertainty of the same summaries computed empirically.
#'
#' A fit of `k` parameters carries `k` degrees of freedom, so at most `k`
#' summaries can be given a non singular covariance. Asking for more is an
#' error. Where the study reported no parameter uncertainty, no covariance is
#' returned and the sample size `n` or a standard error must be supplied
#' through `...` instead, which then treats the summaries as if they had been
#' computed empirically.
#'
#' Where the linearisation the delta method makes is a concern, draw parameter
#' values from the study's reported uncertainty, push each draw through to the
#' summaries, and use [draws_to_multivariate()] on the result.
#'
#' @param family The distribution the study fitted, one of `"lognormal"`,
#'  `"gamma"` or `"weibull"`.
#'
#' @param study A string naming the study the reported fit comes from.
#'
#' @param parameters A named numeric vector of the reported parameters. The
#'  names must be `meanlog` and `sdlog` for a lognormal, `shape` with either
#'  `scale` or `rate` for a gamma, and `shape` and `scale` for a weibull.
#'
#' @param se A numeric vector of the reported standard errors of `parameters`,
#'  in the same order. An alternative to `vcov` where the study reported no
#'  correlation between its parameters. Optional.
#'
#' @param vcov The reported covariance matrix of `parameters`. Must be
#'  symmetric positive definite, and is reordered to match `parameters` when it
#'  has dimension names. Optional.
#'
#' @inheritParams draws_to_multivariate
#'
#' @inheritParams delays_to_multivariate
#'
#' @inherit draws_to_multivariate return
#'
#' @family estimates_data
#' @importFrom checkmate assert_choice assert_matrix
#' @export
#' @examples
#' reported <- parameters_to_multivariate(
#'   "gamma",
#'   c(shape = 4.1, rate = 0.55),
#'   study = "study A",
#'   se = c(0.4, 0.06),
#'   relative_obs_time = 20,
#'   trunc_adjusted = FALSE,
#'   cens_adjusted = 0
#' )
#' as_epidist_estimates_data(reported$data, vcov = reported$vcov)
parameters_to_multivariate <- function(
  family,
  parameters,
  study,
  moments = c("mean", "sd"),
  probs = numeric(0),
  se = NULL,
  vcov = NULL,
  ...
) {
  assert_string(study)
  assert_choice(family, names(.estimates_parameter_sets()))
  parameters <- .assert_estimates_parameters(family, parameters)
  assert_subset(moments, c("mean", "sd"), .var.name = "moments")
  assert_numeric(probs, lower = 0, upper = 1, any.missing = FALSE)
  moments <- intersect(c("mean", "sd"), moments)
  n_summary <- length(moments) + length(probs)
  if (n_summary == 0) {
    cli::cli_abort(
      "Report at least one of {.var moments} and {.var probs}."
    )
  }
  parameter_vcov <- .estimates_parameter_vcov(se, vcov, parameters)
  support <- .estimates_reported_support(...)
  summarise <- function(x) {
    return(.estimates_parameter_summary(
      family, stats::setNames(x, names(parameters)), moments, probs,
      lower = support$lower, cutoff = support$cutoff
    ))
  }
  value <- summarise(parameters)
  reported <- tibble(
    study = study,
    type = c(moments, rep("quantile", length(probs))),
    value = value,
    p = c(rep(NA_real_, length(moments)), probs),
    ...
  )
  if (is.null(parameter_vcov)) {
    return(list(data = reported, vcov = NULL))
  }
  if (n_summary > length(parameters)) {
    cli::cli_abort(paste0(
      "A {.val {family}} fit has {length(parameters)} parameters, so at most ",
      "{length(parameters)} summar{?y/ies} can be given a covariance by the ",
      "delta method, but {n_summary} {?was/were} requested. Report fewer ",
      "summaries, or supply a sample size {.var n} instead of parameter ",
      "uncertainty."
    ))
  }
  jacobian <- .estimates_delta_jacobian(
    summarise, parameters, .estimates_parameter_positive(names(parameters))
  )
  covariance <- jacobian %*% parameter_vcov %*% t(jacobian)
  covariance <- (covariance + t(covariance)) / 2
  if (inherits(try(chol(covariance), silent = TRUE), "try-error")) {
    cli::cli_abort(paste0(
      "The reported parameter uncertainty implies a covariance over the ",
      "summaries that is not positive definite. This happens when the map ",
      "from parameters to the requested summaries is not invertible at the ",
      "reported values."
    ))
  }
  return(list(
    data = reported, vcov = stats::setNames(list(covariance), study)
  ))
}

#' Coerce draws of reported quantities to a numeric matrix
#'
#' @param draws A matrix or data frame of draws.
#'
#' @returns A numeric matrix with one row per draw.
#'
#' @keywords internal
.as_estimates_draws <- function(draws) {
  if (is.data.frame(draws)) {
    numeric_col <- vapply(draws, is.numeric, logical(1))
    if (!all(numeric_col)) {
      cli::cli_abort(paste0(
        "Every column of {.var draws} must be numeric, but ",
        "{.var {names(draws)[!numeric_col]}} {?is/are} not. Select the ",
        "columns holding the reported quantities before passing them in."
      ))
    }
    draws <- as.matrix(draws)
  }
  assert_matrix(draws, mode = "numeric", min.rows = 2, min.cols = 1)
  assert_numeric(as.vector(draws), any.missing = FALSE, finite = TRUE)
  return(draws)
}

#' What each column of a set of draws reports
#'
#' @param type The `type` argument of [draws_to_multivariate()].
#'
#' @param columns The column names of the draws, or `NULL`.
#'
#' @param n_summary The number of columns.
#'
#' @returns A character vector of summary types, one per column.
#'
#' @keywords internal
.estimates_draw_types <- function(type, columns, n_summary) {
  supported <- .estimates_types()
  if (is.null(type)) {
    if (is.null(columns) || !all(columns %in% supported)) {
      cli::cli_abort(paste0(
        "{.var type} must say what each column of {.var draws} reports, ",
        "unless every column is named after a supported type ",
        "({.val {supported}})."
      ))
    }
    return(columns)
  }
  assert_character(type, len = n_summary, any.missing = FALSE)
  assert_subset(type, supported, .var.name = "type")
  return(type)
}

#' The quantile probability of each column of a set of draws
#'
#' @param p The `p` argument of [draws_to_multivariate()].
#'
#' @param type A character vector of summary types, one per column.
#'
#' @returns A numeric vector of probabilities, one per column, with `NA` for
#'  columns that are not quantiles.
#'
#' @keywords internal
.estimates_draw_p <- function(p, type) {
  quantile_col <- which(type == "quantile")
  filled <- rep(NA_real_, length(type))
  if (length(quantile_col) == 0) {
    return(filled)
  }
  if (is.null(p)) {
    cli::cli_abort(paste0(
      "{.var p} must give the probability of each column of {.var draws} ",
      "reporting a quantile."
    ))
  }
  if (length(p) == length(quantile_col)) {
    filled[quantile_col] <- p
  } else if (length(p) == length(type)) {
    filled[quantile_col] <- p[quantile_col]
  } else {
    cli::cli_abort(paste0(
      "{.var p} must have one entry per quantile column ",
      "({length(quantile_col)}) or one per column ({length(type)}), but has ",
      "{length(p)}."
    ))
  }
  assert_numeric(
    filled[quantile_col],
    lower = 0, upper = 1, any.missing = FALSE, .var.name = "p"
  )
  return(filled)
}

#' The parameterisations supported for a reported distribution
#'
#' Each family lists the sets of parameter names a study may report it with.
#' The first set of each family is the one used internally.
#'
#' @returns A named list of character vectors of parameter names.
#'
#' @keywords internal
.estimates_parameter_sets <- function() {
  return(list(
    lognormal = list(c("meanlog", "sdlog")),
    gamma = list(c("shape", "scale"), c("shape", "rate")),
    weibull = list(c("shape", "scale"))
  ))
}

#' The `primarycensored` distribution function name of a reported family
#'
#' @param family The distribution the study fitted.
#'
#' @returns A distribution function name.
#'
#' @keywords internal
.estimates_parameter_dist <- function(family) {
  return(switch(family,
    lognormal = "plnorm",
    gamma = "pgamma",
    weibull = "pweibull"
  ))
}

#' Which reported parameters are constrained to be positive
#'
#' @param names The names of the reported parameters.
#'
#' @returns A logical vector, one entry per parameter.
#'
#' @keywords internal
.estimates_parameter_positive <- function(names) {
  return(names != "meanlog")
}

#' Check the parameters a study reported for its fitted distribution
#'
#' @param family The distribution the study fitted.
#'
#' @param parameters A named numeric vector of the reported parameters.
#'
#' @returns The parameters, ordered as the family expects.
#'
#' @keywords internal
.assert_estimates_parameters <- function(family, parameters) {
  parameters <- unlist(parameters)
  assert_numeric(
    parameters,
    any.missing = FALSE, finite = TRUE, names = "unique",
    .var.name = "parameters"
  )
  supported <- .estimates_parameter_sets()[[family]]
  matched <- Filter(
    function(set) {
      return(setequal(set, names(parameters)))
    },
    supported
  )
  if (length(matched) == 0) {
    wanted <- vapply(supported, paste, character(1), collapse = " and ")
    cli::cli_abort(paste0(
      "A {.val {family}} fit must report {.or {.val {wanted}}}, but ",
      "{.var parameters} holds {.val {names(parameters)}}."
    ))
  }
  parameters <- parameters[matched[[1]]]
  positive <- .estimates_parameter_positive(names(parameters))
  invalid <- names(parameters)[positive & parameters <= 0]
  if (length(invalid) > 0) {
    cli::cli_abort(
      "{.var {invalid}} must be greater than zero."
    )
  }
  return(parameters)
}

#' The range of delays a study's reported distribution describes
#'
#' A study that fitted a distribution to right truncated data without
#' correcting for the truncation reported a distribution with more spread than
#' the delays it saw, because its family carries a tail beyond the point its
#' data stop. Summaries of the reported distribution are therefore taken over
#' the range of delays the study could have seen, which is the same range the
#' meta model computes its implied summaries over. The metadata that gives that
#' range is already on its way onto the returned rows, so it is read from there
#' rather than asked for again.
#'
#' @param ... The metadata columns passed to [parameters_to_multivariate()].
#'
#' @returns A list with the study's `lower` and `cutoff` delays.
#'
#' @keywords internal
.estimates_reported_support <- function(...) {
  metadata <- list(...)
  scalar <- function(name, default) {
    if (!hasName(metadata, name)) {
      return(default)
    }
    value <- unique(metadata[[name]])
    if (length(value) != 1) {
      cli::cli_abort(paste0(
        "{.var {name}} must be a single value, because it describes the one ",
        "study the reported parameters come from."
      ))
    }
    return(value)
  }
  obs_time <- scalar("relative_obs_time", Inf)
  assert_numeric(obs_time, lower = 0, len = 1, any.missing = FALSE)
  adjusted <- as.logical(scalar("trunc_adjusted", is.infinite(obs_time)))
  lower <- scalar("delay_min", 0)
  assert_numeric(lower, lower = 0, len = 1, any.missing = FALSE, finite = TRUE)
  cutoff <- if (isTRUE(adjusted)) Inf else obs_time
  if (lower >= cutoff) {
    cli::cli_abort(paste0(
      "{.var delay_min} must be below the largest delay the study could have ",
      "seen, which is its {.var relative_obs_time}."
    ))
  }
  return(list(lower = lower, cutoff = cutoff))
}

#' The covariance matrix of a study's reported parameters
#'
#' @param se A numeric vector of reported standard errors, or `NULL`.
#'
#' @param vcov A reported covariance matrix, or `NULL`.
#'
#' @param parameters The reported parameters, already checked.
#'
#' @returns A covariance matrix ordered as `parameters`, or `NULL` when the
#'  study reported no parameter uncertainty.
#'
#' @keywords internal
.estimates_parameter_vcov <- function(se, vcov, parameters) {
  if (!is.null(se) && !is.null(vcov)) {
    cli::cli_abort(
      "Supply at most one of {.var se} and {.var vcov}."
    )
  }
  if (!is.null(se)) {
    assert_numeric(
      se,
      lower = 0, len = length(parameters), any.missing = FALSE, finite = TRUE,
      .var.name = "se"
    )
    return(diag(se^2, nrow = length(se)))
  }
  if (is.null(vcov)) {
    return(NULL)
  }
  vcov <- as.matrix(vcov)
  assert_matrix(
    vcov,
    mode = "numeric", nrows = length(parameters),
    ncols = length(parameters), .var.name = "vcov"
  )
  if (!is.null(colnames(vcov))) {
    if (!setequal(colnames(vcov), names(parameters))) {
      cli::cli_abort(paste0(
        "{.var vcov} is named for {.val {colnames(vcov)}} but ",
        "{.var parameters} holds {.val {names(parameters)}}."
      ))
    }
    vcov <- vcov[names(parameters), names(parameters), drop = FALSE]
  }
  if (!isTRUE(all.equal(vcov, t(vcov), tolerance = 1e-8, check.attributes = FALSE))) { # nolint: line_length_linter.
    cli::cli_abort("{.var vcov} must be symmetric.")
  }
  if (inherits(try(chol(vcov), silent = TRUE), "try-error")) {
    cli::cli_abort("{.var vcov} must be positive definite.")
  }
  dimnames(vcov) <- NULL
  return(vcov)
}

#' The summaries a reported distribution implies
#'
#' @param family The distribution the study fitted.
#'
#' @param parameters A named numeric vector of the reported parameters.
#'
#' @param moments Which moments to report, any of `"mean"` and `"sd"`, in that
#'  order.
#'
#' @param probs A numeric vector of probabilities to report quantiles at.
#'
#' @param lower The smallest delay the study counted.
#'
#' @param cutoff The largest delay the study could have seen, or `Inf` where it
#'  adjusted for right truncation.
#'
#' @returns A numeric vector of summaries, the moments first.
#'
#' @keywords internal
.estimates_parameter_summary <- function(
  family,
  parameters,
  moments,
  probs,
  lower = 0,
  cutoff = Inf
) {
  dist_name <- .estimates_parameter_dist(family)
  dist_args <- as.list(parameters)
  if (identical(family, "gamma") && hasName(dist_args, "rate")) {
    dist_args <- list(shape = dist_args$shape, scale = 1 / dist_args$rate)
  }
  pdist <- .pdist(dist_name)
  qdist <- .estimates_qdist(dist_name)
  summaries <- numeric(0)
  if (length(moments) > 0) {
    if (lower == 0 && is.infinite(cutoff)) {
      implied <- .meta_continuous_moments(dist_name, dist_args)
    } else {
      bound <- cutoff
      if (is.infinite(bound)) {
        bound <- do.call(qdist, c(list(p = 1 - 1e-6), dist_args))
      }
      implied <- .meta_trunc_moments(
        dist_name, dist_args, lower = lower, cutoff = bound
      )
    }
    summaries <- unname(implied[moments])
  }
  if (length(probs) > 0) {
    base <- if (lower > 0) {
      do.call(pdist, c(list(q = lower), dist_args))
    } else {
      0
    }
    top <- if (is.finite(cutoff)) {
      do.call(pdist, c(list(q = cutoff), dist_args))
    } else {
      1
    }
    scaled <- base + probs * (top - base)
    summaries <- c(summaries, do.call(qdist, c(list(p = scaled), dist_args)))
  }
  return(summaries)
}

#' The quantile function used for a `primarycensored` distribution name
#'
#' Shares the distribution function lookup with [.pdist()] in `R/gen.R`; only
#' the quantile direction is specific to reported distribution parameters.
#'
#' @inheritParams .pdist
#'
#' @returns The corresponding function from `stats`.
#'
#' @keywords internal
.estimates_qdist <- function(dist) {
  return(switch(dist,
    plnorm = stats::qlnorm,
    pgamma = stats::qgamma,
    pweibull = stats::qweibull,
    get(sub("^p", "q", dist), envir = asNamespace("stats"))
  ))
}

#' A numerical Jacobian of the map from parameters to summaries
#'
#' Uses a central difference with a step relative to each parameter, held away
#' from zero so that a parameter reported as zero still moves. The step of a
#' parameter constrained to be positive is held below half its value, so the
#' lower evaluation stays inside the support.
#'
#' A numerical Jacobian is used because the derivative of a quantile with
#' respect to the shape of a gamma or a weibull has no closed form.
#'
#' @param fn A function of the parameter vector returning the summaries.
#'
#' @param x The reported parameters.
#'
#' @param positive A logical vector marking the parameters constrained to be
#'  positive.
#'
#' @returns A matrix with one row per summary and one column per parameter.
#'
#' @keywords internal
.estimates_delta_jacobian <- function(fn, x, positive) {
  delta <- pmax(abs(x) * 1e-4, 1e-6)
  delta[positive] <- pmin(delta[positive], x[positive] / 2)
  n_summary <- length(fn(x))
  derivative <- vapply(
    seq_along(x),
    function(j) {
      upper <- x
      lower <- x
      upper[j] <- x[j] + delta[j]
      lower[j] <- x[j] - delta[j]
      return((fn(upper) - fn(lower)) / (2 * delta[j]))
    },
    numeric(n_summary)
  )
  return(matrix(derivative, nrow = n_summary, ncol = length(x)))
}
