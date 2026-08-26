#' The number of quadrature intervals used for truncated continuous moments
#'
#' Matches the value hard coded in `inst/stan/meta_model/functions.stan` so
#' that the R and Stan implementations agree.
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad <- function() {
  return(100L)
}

#' The distribution function used for a `primarycensored` distribution name
#'
#' @param dist A `primarycensored` distribution function name, for example
#'  `"plnorm"`.
#'
#' @returns The corresponding function from `stats`.
#'
#' @keywords internal
.meta_pdist <- function(dist) {
  return(get(dist, envir = asNamespace("stats")))
}

#' The primary event distribution implied by a growth rate
#'
#' A growth rate of zero corresponds to a uniform primary event within its
#' censoring window. Any other value uses the exponential growth primary
#' distribution from `primarycensored`.
#'
#' @param growth_rate The exponential growth rate of primary events.
#'
#' @returns A list with elements `dprimary` and `dprimary_args`.
#'
#' @keywords internal
.meta_primary <- function(growth_rate) {
  if (growth_rate == 0) {
    return(list(dprimary = stats::dunif, dprimary_args = list()))
  }
  return(list(
    dprimary = primarycensored::dexpgrowth,
    dprimary_args = list(r = growth_rate)
  ))
}

#' The primary censored distribution function, guarded against underflow
#'
#' Primary distributions without an analytical solution are integrated
#' numerically, which can return a non finite or negative cumulative
#' probability deep in the lower tail. Those cases carry negligible
#' probability and are treated as zero, matching the guard in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param q A numeric vector of delays.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param pwindow The primary censoring window width.
#'
#' @param growth_rate The exponential growth rate of primary events.
#'
#' @returns A numeric vector of cumulative probabilities.
#'
#' @keywords internal
.meta_pcens_cdf <- function(q, dist, args, pwindow, growth_rate) {
  primary <- .meta_primary(growth_rate)
  cdf <- do.call(
    primarycensored::pprimarycensored,
    c(
      list(
        q = q,
        pdist = .meta_pdist(dist),
        pwindow = pwindow,
        dprimary = primary$dprimary,
        dprimary_args = primary$dprimary_args
      ),
      args
    )
  )
  cdf[!is.finite(cdf) | cdf < 0] <- 0
  return(pmin(cdf, 1))
}

#' The discrete delay distribution a naive study would observe
#'
#' Builds the probability mass function of the interval censored delays a study
#' that took date differences directly would have summarised. The grid runs
#' over delays of `0`, `swindow`, `2 * swindow`, and so on up to the largest
#' multiple of `swindow` whose upper bound is within `cutoff`, and is
#' renormalised so that it conditions on delays falling within the grid. This
#' renormalisation is what applies the study's right truncation, and it
#' discretises the truncation point to the nearest grid boundary.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param cutoff The grid cutoff, either the study observation time or
#'  `max_delay`.
#'
#' @param pwindow,swindow The primary and secondary censoring window widths.
#'
#' @param growth_rate The exponential growth rate of primary events.
#'
#' @returns A numeric vector of probabilities summing to one.
#'
#' @keywords internal
.meta_grid_pmf <- function(dist, args, cutoff, pwindow, swindow, growth_rate) {
  n_grid <- floor(cutoff / swindow)
  boundary <- seq_len(n_grid) * swindow
  cdf <- .meta_pcens_cdf(boundary, dist, args, pwindow, growth_rate)
  mass <- diff(c(0, cdf))
  return(mass / sum(mass))
}

#' Summarise a distribution from its mean and central moments
#'
#' The kurtosis is needed as well as the mean and standard deviation because
#' the sampling error of a reported standard deviation depends on it. See
#' [.meta_summary_terms()].
#'
#' @param mean The mean of the distribution.
#'
#' @param variance The variance of the distribution.
#'
#' @param fourth The fourth central moment of the distribution.
#'
#' @returns A named numeric vector with elements `mean`, `sd` and `kurtosis`.
#'
#' @keywords internal
.meta_moment_vector <- function(mean, variance, fourth) {
  variance <- max(variance, 1e-10)
  return(c(
    mean = mean,
    sd = sqrt(variance),
    kurtosis = max(fourth / variance^2, 1)
  ))
}

#' Summarise a distribution from its first four raw moments
#'
#' @param moments A numeric vector of the first four raw moments.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_central_from_raw <- function(moments) {
  m1 <- moments[1]
  variance <- moments[2] - m1^2
  fourth <- moments[4] -
    4 * m1 * moments[3] +
    6 * m1^2 * moments[2] -
    3 * m1^4
  return(.meta_moment_vector(m1, variance, fourth))
}

#' Summaries of a discrete delay grid
#'
#' @param mass A vector of grid probabilities from [.meta_grid_pmf()].
#'
#' @param swindow The secondary censoring window width, which is also the grid
#'  spacing.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_grid_moments <- function(mass, swindow) {
  delay <- (seq_along(mass) - 1) * swindow
  m1 <- sum(mass * delay)
  centred <- delay - m1
  variance <- sum(mass * centred^2)
  fourth <- sum(mass * centred^4)
  return(.meta_moment_vector(m1, variance, fourth))
}

#' Analytic summaries of a delay distribution
#'
#' The mean and standard deviation mirror the formulas used by [add_mean_sd()].
#'
#' @inheritParams .meta_grid_pmf
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_continuous_moments <- function(dist, args) {
  if (identical(dist, "plnorm")) {
    var_log <- args$sdlog^2
    delay_mean <- exp(args$meanlog + var_log / 2)
    variance <- delay_mean^2 * expm1(var_log)
    kurtosis <- exp(4 * var_log) +
      2 * exp(3 * var_log) +
      3 * exp(2 * var_log) -
      3
    return(.meta_moment_vector(delay_mean, variance, kurtosis * variance^2))
  }
  if (identical(dist, "pgamma")) {
    variance <- args$shape * args$scale^2
    return(.meta_moment_vector(
      args$shape * args$scale,
      variance,
      (3 + 6 / args$shape) * variance^2
    ))
  }
  if (identical(dist, "pweibull")) {
    g <- gamma(1 + seq_len(4) / args$shape)
    variance <- args$scale^2 * (g[2] - g[1]^2)
    fourth <- args$scale^4 *
      (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] - 3 * g[1]^4)
    return(.meta_moment_vector(args$scale * g[1], variance, fourth))
  }
  return(cli::cli_abort(
    "Summary estimates are not supported for the {.val {dist}} distribution."
  ))
}

#' Summaries implied by a distribution function evaluated on a grid
#'
#' Uses Simpson's rule on the survival integrals
#' \eqn{\int_0^D k t^{k - 1} (F(D) - F(t)) dt}, which are the first four raw
#' moments of the distribution truncated at the cutoff. Matches the
#' implementation in `inst/stan/meta_model/functions.stan`.
#'
#' @param cdf The distribution function at `.meta_n_quad() + 1` equally spaced
#'  points running from zero to `cutoff`.
#'
#' @param cutoff The right truncation point.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_survival_moments <- function(cdf, cutoff) {
  n_quad <- length(cdf) - 1
  quad <- seq(0, cutoff, length.out = n_quad + 1)
  weight <- c(1, rep_len(c(4, 2), n_quad - 1), 1)
  tail_prob <- cdf[n_quad + 1] - cdf
  raw_moments <- vapply(
    seq_len(4),
    function(k) {
      integrand <- k * quad^(k - 1) * tail_prob
      integral <- sum(weight * integrand) * cutoff / (3 * n_quad)
      return(integral / cdf[n_quad + 1])
    },
    numeric(1)
  )
  return(.meta_central_from_raw(raw_moments))
}

#' Summaries of a right truncated delay distribution
#'
#' @inheritParams .meta_grid_pmf
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_trunc_moments <- function(dist, args, cutoff) {
  quad <- seq(0, cutoff, length.out = .meta_n_quad() + 1)
  cdf <- do.call(.meta_pdist(dist), c(list(q = quad), args))
  return(.meta_survival_moments(cdf, cutoff))
}

#' Summaries of a right truncated primary censored delay distribution
#'
#' The estimand is the delay plus the primary event offset within its window,
#' conditioned on falling below the cutoff.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_pcens_trunc_moments <- function(
  dist,
  args,
  cutoff,
  pwindow,
  growth_rate
) {
  quad <- seq(0, cutoff, length.out = .meta_n_quad() + 1)
  cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
  return(.meta_survival_moments(cdf, cutoff))
}

#' Add an independent uniform primary window to a set of summaries
#'
#' The uniform single interval approximation leaves the primary interval
#' uncorrected, so the study effectively summarised the delay plus an
#' independent draw from a uniform distribution over the primary window. This
#' convolution is exact when the study also adjusted for right truncation and
#' the primary events were uniform within their window.
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @param pwindow The primary censoring window width.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_add_uniform <- function(moments, pwindow) {
  var_delay <- moments[["sd"]]^2
  fourth_delay <- moments[["kurtosis"]] * var_delay^2
  var_window <- pwindow^2 / 12
  fourth_window <- pwindow^4 / 80
  return(.meta_moment_vector(
    moments[["mean"]] + pwindow / 2,
    var_delay + var_window,
    fourth_delay + 6 * var_delay * var_window + fourth_window
  ))
}

#' The summaries a study using a given procedure would report
#'
#' Forward models the summaries that a study would converge to given the biases
#' in its estimation procedure. See
#' [as_epidist_estimates_data.data.frame()] for what the adjustment codes mean.
#'
#' Under the uniform single interval approximation (`cens_adjusted` of 2) the
#' study summarised the delay plus the uncorrected primary event offset. Where
#' the study also adjusted for right truncation and the primary events were
#' uniform within their window this is the analytic convolution, which adds
#' `pwindow / 2` to the mean and `pwindow^2 / 12` to the variance. Otherwise
#' the moments of the primary censored delay, truncated at `cutoff`, are used
#' directly.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @param trunc_adjusted 1 if the study adjusted for right truncation, 0
#'  otherwise.
#'
#' @param cens_adjusted The censoring adjustment code, one of 0, 1, or 2.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_implied_moments <- function(
  dist,
  args,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate
) {
  if (cens_adjusted == 0) {
    mass <- .meta_grid_pmf(
      dist, args, cutoff, pwindow, swindow, growth_rate
    )
    return(.meta_grid_moments(mass, swindow))
  }
  if (cens_adjusted == 2) {
    if (trunc_adjusted == 1 && growth_rate == 0) {
      return(.meta_add_uniform(.meta_continuous_moments(dist, args), pwindow))
    }
    return(.meta_pcens_trunc_moments(dist, args, cutoff, pwindow, growth_rate))
  }
  if (trunc_adjusted == 1) {
    return(.meta_continuous_moments(dist, args))
  }
  return(.meta_trunc_moments(dist, args, cutoff))
}

#' The cumulative probability a study using a given procedure would report
#'
#' Evaluates the distribution function of the biased estimand at a reported
#' quantile value. Working on the probability scale avoids inverting the
#' distribution function, which has no closed form on the discrete grid.
#'
#' For a naive study (`cens_adjusted` of 0) the estimand is discrete, so the
#' step distribution function is replaced by the continuity corrected version
#' that interpolates it linearly through the mid points of the grid cells.
#' Without this correction a quantile of day resolution data, which must land
#' on a jump of the step function, biases the implied probability upwards by
#' several sampling standard errors.
#'
#' For the uniform single interval approximation (`cens_adjusted` of 2) the
#' distribution function of the primary censored delay is used, so that it
#' matches the moments used for reported means and standard deviations.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A probability.
#'
#' @keywords internal
.meta_implied_prob <- function(
  y,
  dist,
  args,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate
) {
  if (cens_adjusted == 0) {
    n_grid <- floor(cutoff / swindow)
    cell <- floor(y / swindow + 0.5)
    frac <- y / swindow + 0.5 - cell
    if (cell < 0) {
      return(0)
    }
    if (cell >= n_grid) {
      return(1)
    }
    grid_cdf <- c(0, cumsum(.meta_grid_pmf(
      dist, args, cutoff, pwindow, swindow, growth_rate
    )))
    return(grid_cdf[cell + 1] * (1 - frac) + grid_cdf[cell + 2] * frac)
  }
  if (y <= 0) {
    return(0)
  }
  if (cens_adjusted == 2) {
    cdf <- function(q) {
      return(.meta_pcens_cdf(q, dist, args, pwindow, growth_rate))
    }
  } else {
    cdf <- function(q) {
      return(do.call(.meta_pdist(dist), c(list(q = q), args)))
    }
  }
  if (trunc_adjusted == 1) {
    return(cdf(y))
  }
  if (y >= cutoff) {
    return(1)
  }
  return(cdf(y) / cdf(cutoff))
}

#' Extract the meta model slots for a single row
#'
#' @param i The row index.
#'
#' @param prep A `brms` prep object.
#'
#' @returns A named list of the observation type, study metadata and reported
#'  value for row `i`.
#'
#' @keywords internal
.meta_row_slots <- function(i, prep) {
  return(list(
    obs_type = prep$data$vint1[i],
    study_n = prep$data$vint2[i],
    trunc_adjusted = prep$data$vint3[i],
    cens_adjusted = prep$data$vint4[i],
    cutoff = prep$data$vreal1[i],
    pwindow = prep$data$vreal2[i],
    swindow = prep$data$vreal3[i],
    value = prep$data$vreal4[i],
    report_se = prep$data$vreal5[i],
    quantile_p = prep$data$vreal6[i],
    growth_rate = prep$data$vreal7[i]
  ))
}

#' The sampling standard error of a reported standard deviation
#'
#' Uses the asymptotic standard error of the sample standard deviation,
#' \eqn{\sigma \sqrt{(\kappa - 1) / (4 n)}}, where \eqn{\kappa} is the kurtosis
#' of the estimand the study was summarising. The normal theory expression
#' \eqn{\sigma / \sqrt{2 (n - 1)}} is not used because it is far too narrow for
#' the skewed distributions delays usually follow.
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @param study_n The number of delays the standard deviation was computed
#'  from.
#'
#' @returns The standard error of the reported standard deviation.
#'
#' @keywords internal
.meta_sd_se <- function(moments, study_n) {
  excess <- max(moments[["kurtosis"]] - 1, 1e-10)
  return(moments[["sd"]] * sqrt(excess / (4 * study_n)))
}

#' The implied summary and its standard error for one summary row and one draw
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters for a single draw.
#'
#' @returns A named numeric vector with elements `observed`, `implied` and
#'  `se`.
#'
#' @keywords internal
.meta_summary_terms <- function(slots, dist, args) {
  if (slots$obs_type == 4L) {
    implied <- .meta_implied_prob(
      slots$value, dist, args, slots$cutoff, slots$pwindow, slots$swindow,
      slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate
    )
    se <- sqrt(slots$quantile_p * (1 - slots$quantile_p) / slots$study_n)
    observed <- slots$quantile_p
  } else {
    moments <- .meta_implied_moments(
      dist, args, slots$cutoff, slots$pwindow, slots$swindow,
      slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate
    )
    observed <- slots$value
    if (slots$obs_type == 2L) {
      implied <- moments[["mean"]]
      se <- moments[["sd"]] / sqrt(slots$study_n)
    } else {
      implied <- moments[["sd"]]
      se <- .meta_sd_se(moments, slots$study_n)
    }
  }
  if (slots$report_se > 0) {
    se <- slots$report_se
  }
  return(c(observed = unname(observed), implied = unname(implied), se = se))
}

#' Check that a family is supported for meta model summary rows
#'
#' @inheritParams epidist_family
#'
#' @returns The `primarycensored` distribution function name, or `NULL` if the
#'  family is not supported.
#'
#' @keywords internal
.meta_summary_dist <- function(family) {
  dist_name <- tryCatch(
    primarycensored::pcd_dist_name(tolower(family$family)),
    error = function(e) tolower(family$family)
  )
  if (!dist_name %in% .get_supported_dists()) {
    cli::cli_inform(
      c(
        "!" = paste0(
          "Summary rows of the meta model are not supported in R for the ",
          "{dist_name} distribution, so their log likelihood and posterior ",
          "predictions are returned as NA. Model fitting is unaffected."
        )
      ),
      .frequency = "once",
      .frequency_id = paste0("epidist_meta_summary_", dist_name)
    )
    return(NULL)
  }
  return(dist_name)
}

#' Create a function to calculate the meta model log likelihood
#'
#' Individual level rows use the marginal model log likelihood created by
#' [epidist_gen_log_lik()]. Summary rows use the normal approximations
#' described in [as_epidist_meta_model()], evaluated at the implied summary for
#' each posterior draw.
#'
#' @inheritParams epidist_family
#'
#' @returns A function that calculates the log likelihood for a single
#'  observation. The prep object must have the meta model `vint` and `vreal`
#'  slots.
#'
#' @seealso [brms::log_lik()] for details on the brms log likelihood interface.
#'
#' @family meta_model
#' @importFrom purrr map_dbl
#' @export
epidist_gen_meta_log_lik <- function(family) {
  marginal_log_lik <- epidist_gen_log_lik(family)
  dist_name <- .meta_summary_dist(family)

  .log_lik <- function(i, prep) {
    if (prep$data$vint1[i] == 1) {
      return(marginal_log_lik(i, prep))
    }
    if (is.null(dist_name)) {
      return(rep(NA_real_, prep$ndraws))
    }
    slots <- .meta_row_slots(i, prep)
    dist_args <- .get_supported_dist_args(dist_name, prep, i)
    lpdf <- map_dbl(dist_args, function(args) {
      summaries <- .meta_summary_terms(slots, dist_name, args)
      return(stats::dnorm(
        summaries[["observed"]], summaries[["implied"]],
        summaries[["se"]],
        log = TRUE
      ))
    })
    lpdf <- brms:::log_lik_weight(lpdf, i = i, prep = prep) # nolint
    return(lpdf)
  }

  return(.log_lik)
}

#' Create a function to draw from the meta model posterior predictive
#' distribution
#'
#' Individual level rows are predicted as in the marginal model using
#' [epidist_gen_posterior_predict()]. For summary rows the predicted quantity
#' is the reported summary itself, that is a simulated reported mean, standard
#' deviation, or, for quantile rows, cumulative probability at the reported
#' value. Predictions for summary rows are therefore not on the delay scale and
#' should not be compared directly with individual level predictions. They come
#' from the normal approximations described in [as_epidist_meta_model()], so a
#' predicted cumulative probability for a quantile row can fall outside
#' \[0, 1\] when the study sample size is small.
#'
#' @inheritParams epidist_family
#'
#' @returns A function that takes a `prep` argument from brms and returns a
#'  matrix of posterior predictions.
#'
#' @seealso [brms::posterior_predict()] for details on how this is used within
#'  `brms`.
#'
#' @family meta_model
#' @importFrom purrr map_dbl
#' @export
epidist_gen_meta_predict <- function(family) {
  marginal_predict <- epidist_gen_posterior_predict(family)
  dist_name <- .meta_summary_dist(family)

  .predict <- function(i, prep, ...) {
    if (prep$data$vint1[i] == 1) {
      return(marginal_predict(i, prep, ...))
    }
    if (is.null(dist_name)) {
      return(as.matrix(rep(NA_real_, prep$ndraws)))
    }
    slots <- .meta_row_slots(i, prep)
    dist_args <- .get_supported_dist_args(dist_name, prep, i)
    draws <- map_dbl(dist_args, function(args) {
      summaries <- .meta_summary_terms(slots, dist_name, args)
      return(stats::rnorm(1, summaries[["implied"]], summaries[["se"]]))
    })
    return(as.matrix(draws))
  }

  return(.predict)
}
