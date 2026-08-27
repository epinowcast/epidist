#' Report a study that published summaries of its delays
#'
#' Builds the rows [as_epidist_meta_model()] takes from the summaries a study
#' published in wide form, one argument per kind of summary.
#'
#' Give the uncertainty of each summary through `se`, or the number of delays
#' the study summarised through `n`, which the model uses to derive a sampling
#' uncertainty instead. One of the two is needed for every row.
#'
#' @param study A string naming the study.
#'
#' @param mean The reported mean delay. Optional.
#'
#' @param sd The reported standard deviation of the delays. Optional.
#'
#' @param quantiles A numeric vector of reported quantiles. Optional.
#'
#' @param probs The probabilities of `quantiles`, in the same order. Required
#'  where `quantiles` is given.
#'
#' @param se A numeric vector of the reported standard errors of the
#'  summaries, ordered mean, standard deviation, then quantiles, skipping any
#'  that was not reported. Optional.
#'
#' @param n The number of delays the study summarised. Optional.
#'
#' @param ... Study metadata, as documented in
#'  [as_epidist_estimates_data.data.frame()].
#'
#' @returns An `epidist_estimates_data` object.
#'
#' @family estimates_data
#' @importFrom checkmate assert_string assert_numeric
#' @importFrom tibble tibble
#' @export
#' @examples
#' epidist_estimates_summaries(
#'   "study A",
#'   mean = 7.5, sd = 3.6, n = 120,
#'   relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0
#' )
epidist_estimates_summaries <- function(
  study,
  mean = NULL,
  sd = NULL,
  quantiles = NULL,
  probs = NULL,
  se = NULL,
  n = NULL,
  ...
) {
  assert_string(study)
  assert_numeric(mean, len = 1, null.ok = TRUE, finite = TRUE)
  assert_numeric(sd, len = 1, null.ok = TRUE, finite = TRUE)
  assert_numeric(quantiles, null.ok = TRUE, any.missing = FALSE)
  if (is.null(quantiles)) {
    probs <- numeric(0)
  }
  assert_numeric(
    probs,
    len = length(quantiles), lower = 0, upper = 1, any.missing = FALSE,
    .var.name = "probs"
  )
  value <- c(mean, sd, quantiles)
  if (length(value) == 0) {
    cli::cli_abort(
      "Report at least one of {.var mean}, {.var sd} and {.var quantiles}."
    )
  }
  type <- c(
    rep("mean", length(mean)), rep("sd", length(sd)),
    rep("quantile", length(quantiles))
  )
  return(.estimates_from_summaries(
    study = study,
    value = value,
    type = type,
    p = c(rep(NA_real_, length(mean) + length(sd)), probs),
    se = se,
    n = n,
    ...
  ))
}

#' Report a study that published the parameters of a distribution it fitted
#'
#' Studies often publish the parameters of a distribution they fitted rather
#' than summaries of the delays themselves. This converts those parameters into
#' the summaries the fitted distribution implies, which is what
#' [as_epidist_meta_model()] fits to.
#'
#' # What the reported parameters are taken to mean
#'
#' The reported parameters describe the distribution the study's own estimation
#' procedure converged to. Where the procedure was correct, that is the delay
#' distribution itself. Where it was not, it is the biased distribution the
#' procedure targeted, which is exactly what the meta model forward models from
#' the study metadata. Converting to summaries therefore covers both cases with
#' one route, and the study metadata documented in
#' [as_epidist_estimates_data.data.frame()] is supplied in the same way as for
#' any other summary.
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
#' Supply the standard errors the study reported on its parameters through
#' `se`. They are carried onto the summary scale by the delta method, using a
#' numerical Jacobian of the map from parameters to summaries, and each summary
#' is reported with its own standard error.
#'
#' The correlation the delta method implies between those summaries is not
#' kept. Summaries of a two parameter fit are deterministic functions of two
#' numbers, so their joint distribution is degenerate and reporting more than
#' two of them with a covariance is not possible. A study that published a full
#' parameter covariance and wants the correlation kept can draw from it, push
#' each draw through to the summaries, and use [as_epidist_multivariate()],
#' which needs no linearisation.
#'
#' @param study A string naming the study.
#'
#' @param family The distribution the study fitted, one of `"lognormal"`,
#'  `"gamma"` or `"weibull"`.
#'
#' @param parameters A named numeric vector of the reported parameters. The
#'  names must be `meanlog` and `sdlog` for a lognormal, `shape` with either
#'  `scale` or `rate` for a gamma, and `shape` and `scale` for a weibull.
#'
#' @param moments Which moments to report, any of `"mean"` and `"sd"`.
#'
#' @param probs A numeric vector of probabilities to report quantiles at.
#'
#' @param se A numeric vector of the reported standard errors of `parameters`,
#'  in the same order. Optional.
#'
#' @param n The number of delays the study fitted. Optional, and used where no
#'  `se` is given.
#'
#' @param ... Study metadata, as documented in
#'  [as_epidist_estimates_data.data.frame()].
#'
#' @returns An `epidist_estimates_data` object.
#'
#' @family estimates_data
#' @importFrom checkmate assert_choice assert_subset
#' @export
#' @examples
#' epidist_estimates_parameters(
#'   "study A",
#'   family = "gamma",
#'   parameters = c(shape = 4.1, rate = 0.55),
#'   se = c(0.4, 0.06),
#'   relative_obs_time = 20,
#'   trunc_adjusted = FALSE,
#'   cens_adjusted = 0
#' )
epidist_estimates_parameters <- function(
  study,
  family,
  parameters,
  moments = c("mean", "sd"),
  probs = numeric(0),
  se = NULL,
  n = NULL,
  ...
) {
  assert_string(study)
  assert_choice(family, names(.estimates_parameter_sets()))
  parameters <- .assert_estimates_parameters(family, parameters)
  moments <- .estimates_moments(moments, probs)
  support <- .estimates_reported_support(...)
  summarise <- function(x) {
    return(.estimates_parameter_summary(
      family, stats::setNames(x, names(parameters)), moments, probs,
      lower = support$lower, cutoff = support$cutoff
    ))
  }
  summary_se <- NULL
  if (!is.null(se)) {
    assert_numeric(
      se,
      lower = 0, len = length(parameters), any.missing = FALSE, finite = TRUE,
      .var.name = "se"
    )
    jacobian <- .estimates_delta_jacobian(
      summarise, parameters, .estimates_parameter_positive(names(parameters))
    )
    summary_se <- sqrt(rowSums((jacobian %*% diag(se^2, length(se))) *
      jacobian))
  }
  return(.estimates_from_summaries(
    study = study,
    value = summarise(parameters),
    type = c(moments, rep("quantile", length(probs))),
    p = c(rep(NA_real_, length(moments)), probs),
    se = summary_se,
    n = n,
    ...
  ))
}

#' Build an `epidist_estimates_data` object from one study's summaries
#'
#' @param study A string naming the study.
#'
#' @param value A numeric vector of reported summaries.
#'
#' @param type A character vector of summary types, one per `value`.
#'
#' @param p A numeric vector of quantile probabilities, one per `value`.
#'
#' @param se A numeric vector of reported standard errors, or `NULL`.
#'
#' @param n The number of delays the study summarised, or `NULL`.
#'
#' @param ... Study metadata.
#'
#' @returns An `epidist_estimates_data` object.
#'
#' @keywords internal
#' @importFrom tibble tibble
.estimates_from_summaries <- function(study, value, type, p, se, n, ...) {
  assert_numeric(
    se,
    lower = 0, len = length(value), any.missing = FALSE, finite = TRUE,
    null.ok = TRUE, .var.name = "se"
  )
  assert_numeric(n, lower = 1, len = 1, null.ok = TRUE, .var.name = "n")
  if (is.null(se)) {
    se <- NA_real_
  }
  if (is.null(n)) {
    n <- NA_real_
  }
  return(as_epidist_estimates_data(tibble(
    study = study,
    type = type,
    value = unname(value),
    se = unname(se),
    n = n,
    p = p,
    ...
  )))
}

#' The moments a reported fit is summarised by
#'
#' @param moments Which moments to report.
#'
#' @param probs A numeric vector of probabilities to report quantiles at.
#'
#' @returns The moments, ordered as the summary vector expects.
#'
#' @keywords internal
.estimates_moments <- function(moments, probs) {
  assert_subset(moments, c("mean", "sd"), .var.name = "moments")
  assert_numeric(probs, lower = 0, upper = 1, any.missing = FALSE)
  moments <- intersect(c("mean", "sd"), moments)
  if (length(moments) + length(probs) == 0) {
    cli::cli_abort(
      "Report at least one of {.var moments} and {.var probs}."
    )
  }
  return(moments)
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
#' @param ... The metadata columns passed to
#'  [epidist_estimates_parameters()].
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
