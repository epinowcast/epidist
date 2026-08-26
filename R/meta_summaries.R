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

#' The smallest cumulative probability standard error the model will use
#'
#' A quantile standard error supplied on the delay scale is converted to the
#' cumulative probability scale by multiplying it by the density of the biased
#' estimand at the reported value. That density is zero beyond the support of a
#' discrete estimand and vanishingly small far into a tail, either of which
#' would give a degenerate likelihood, so the converted standard error is held
#' at or above this value.
#'
#' @returns A probability scale standard error.
#'
#' @keywords internal
.meta_min_prob_se <- function() {
  return(1e-6)
}

#' Whether accrual weighting applies to a summary row
#'
#' The truncation design only matters for a study that did not adjust for right
#' truncation, because a study that did has already removed the effect the
#' design would have had.
#'
#' @param trunc_adjusted 1 if the study adjusted for right truncation, 0
#'  otherwise.
#'
#' @param trunc_design 0 for a cohort design, 1 for an accrual design.
#'
#' @returns 1 if the accrual weight applies, 0 otherwise.
#'
#' @keywords internal
.meta_accrual_flag <- function(trunc_adjusted, trunc_design) {
  return(as.integer(trunc_adjusted != 1 && trunc_design == 1))
}

#' The log follow up available to a delay under an accrual design
#'
#' A study that collected primary events over a window of length `window` and
#' stopped at its calendar end could only observe a delay of `d` for the
#' primary events that occurred at least `d` before the stop. With primary
#' events arriving at a rate proportional to \eqn{\exp(r t)} the amount of such
#' follow up is
#' \eqn{w(d) = \int_0^{window - d} \exp(r t) \text{d}t =
#' (\exp(r (window - d)) - 1) / r}, which tends to `window - d` as \eqn{r} tends
#' to zero. This is the dynamical bias of Park et al. (2024); for a long window
#' and a growing epidemic it approaches an exponential tilt of the delay
#' distribution by \eqn{\exp(-r d)}.
#'
#' Working on the log scale keeps the weight finite for a fast growing epidemic
#' observed over a long window, where the weight itself would overflow.
#'
#' @param d A numeric vector of delays.
#'
#' @param window The length of the collection window.
#'
#' @param growth_rate The exponential growth rate of primary events.
#'
#' @returns A numeric vector of log follow up times.
#'
#' @keywords internal
.meta_log_accrual_weight <- function(d, window, growth_rate) {
  remaining <- pmax(window - d, 0)
  if (growth_rate == 0) {
    return(log(remaining))
  }
  if (growth_rate > 0) {
    scaled <- growth_rate * remaining
    return(scaled + log(-expm1(-scaled)) - log(growth_rate))
  }
  return(log(-expm1(growth_rate * remaining)) - log(-growth_rate))
}

#' The follow up available to a delay under an accrual design, up to a constant
#'
#' Every use of the accrual weight renormalises afterwards, so the weights are
#' returned relative to their largest value to avoid overflow.
#'
#' @inheritParams .meta_log_accrual_weight
#'
#' @returns A numeric vector of relative weights with a maximum of one.
#'
#' @keywords internal
.meta_accrual_weight <- function(d, window, growth_rate) {
  log_weight <- .meta_log_accrual_weight(d, window, growth_rate)
  return(exp(log_weight - max(log_weight)))
}

#' Reweight a distribution function for an accrual design
#'
#' Weights the probability mass between consecutive quadrature nodes by the
#' follow up available at the midpoint of the interval, then renormalises, so
#' that the returned distribution function is that of the delays a study
#' collecting up to a calendar stop would have seen. The midpoint is used
#' rather than a node because it makes the quadrature second order accurate.
#'
#' The follow up available to a primary event depends on the calendar time of
#' the event itself, which is only known to within its censoring window. Where
#' the quantity being weighted already includes the offset of the primary event
#' within that window, as it does for the uniform single interval
#' approximation, `weight_offset` shifts the weight so that it is evaluated at
#' the underlying primary event time. Averaging over the window makes the shift
#' half its width. Without it the follow up is systematically half a window
#' short, which biases the implied summaries downwards.
#'
#' @param cdf The distribution function at equally spaced nodes running from
#'  zero to `cutoff`.
#'
#' @param cutoff The length of the collection window.
#'
#' @param growth_rate The exponential growth rate of primary events.
#'
#' @param weight_offset The amount by which the quantity being weighted
#'  overstates the time from the primary event's censoring window to the
#'  secondary event.
#'
#' @returns A distribution function at the same nodes, running from zero to
#'  one.
#'
#' @keywords internal
.meta_accrual_reweight <- function(
  cdf,
  cutoff,
  growth_rate,
  weight_offset = 0
) {
  n_quad <- length(cdf) - 1
  midpoint <- (seq_len(n_quad) - 0.5) * cutoff / n_quad - weight_offset
  mass <- diff(cdf) * .meta_accrual_weight(midpoint, cutoff, growth_rate)
  total <- sum(mass)
  if (!is.finite(total) || total <= 0) {
    return(cdf)
  }
  return(c(0, cumsum(mass)) / total)
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
#' Under an accrual design the cell masses are additionally weighted by the
#' follow up available to the delay each cell records, which is the delay at
#' its lower edge, before renormalising.
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
#' @param accrual 1 to apply the accrual weight, 0 otherwise.
#'
#' @returns A numeric vector of probabilities summing to one.
#'
#' @keywords internal
.meta_grid_pmf <- function(
  dist,
  args,
  cutoff,
  pwindow,
  swindow,
  growth_rate,
  accrual = 0L
) {
  n_grid <- floor(cutoff / swindow)
  boundary <- seq_len(n_grid) * swindow
  cdf <- .meta_pcens_cdf(boundary, dist, args, pwindow, growth_rate)
  mass <- diff(c(0, cdf))
  if (accrual == 1L) {
    mass <- mass *
      .meta_accrual_weight(boundary - swindow, cutoff, growth_rate)
  }
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
#' Under an accrual design the quadrature is reweighted by the follow up
#' available to each delay before the moments are taken, which is exact for a
#' study that adjusted for censoring because the weight then applies to the
#' delay itself.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_trunc_moments <- function(
  dist,
  args,
  cutoff,
  growth_rate = 0,
  accrual = 0L
) {
  quad <- seq(0, cutoff, length.out = .meta_n_quad() + 1)
  cdf <- do.call(.meta_pdist(dist), c(list(q = quad), args))
  if (accrual == 1L) {
    cdf <- .meta_accrual_reweight(cdf, cutoff, growth_rate)
  }
  return(.meta_survival_moments(cdf, cutoff))
}

#' Summaries of a right truncated primary censored delay distribution
#'
#' The estimand is the delay plus the primary event offset within its window,
#' conditioned on falling below the cutoff. Under an accrual design the
#' quadrature is reweighted by the follow up available to each delay, offset by
#' half a primary window because the estimand already includes the primary
#' event offset. See [.meta_accrual_reweight()].
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
  growth_rate,
  accrual = 0L
) {
  quad <- seq(0, cutoff, length.out = .meta_n_quad() + 1)
  cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
  if (accrual == 1L) {
    cdf <- .meta_accrual_reweight(cdf, cutoff, growth_rate, pwindow / 2)
  }
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
#' Under midpoint imputation (`cens_adjusted` of 3) the study assigned each
#' delay to the centre of the interval it was observed in, so the estimand is
#' the naive discrete grid shifted up by `swindow / 2`. The shift moves the
#' mean and leaves every central moment unchanged.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @param trunc_adjusted 1 if the study adjusted for right truncation, 0
#'  otherwise.
#'
#' @param cens_adjusted The censoring adjustment code, one of 0, 1, 2, or 3.
#'
#' @param trunc_design 0 for a cohort design, 1 for an accrual design.
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
  growth_rate,
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 0 || cens_adjusted == 3) {
    mass <- .meta_grid_pmf(
      dist, args, cutoff, pwindow, swindow, growth_rate, accrual
    )
    moments <- .meta_grid_moments(mass, swindow)
    if (cens_adjusted == 3) {
      moments[["mean"]] <- moments[["mean"]] + swindow / 2
    }
    return(moments)
  }
  if (cens_adjusted == 2) {
    if (trunc_adjusted == 1 && growth_rate == 0) {
      return(.meta_add_uniform(.meta_continuous_moments(dist, args), pwindow))
    }
    return(.meta_pcens_trunc_moments(
      dist, args, cutoff, pwindow, growth_rate, accrual
    ))
  }
  if (trunc_adjusted == 1) {
    return(.meta_continuous_moments(dist, args))
  }
  return(.meta_trunc_moments(dist, args, cutoff, growth_rate, accrual))
}

#' The continuity corrected distribution function of a discrete delay grid
#'
#' The step distribution function of the grid is replaced by the version that
#' interpolates it linearly through the mid points of its cells. Without this
#' correction a quantile of day resolution data, which must land on a jump of
#' the step function, biases the implied probability upwards by several
#' sampling standard errors.
#'
#' @param y The delay to evaluate the distribution function at.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns A probability.
#'
#' @keywords internal
.meta_grid_prob <- function(
  y,
  dist,
  args,
  cutoff,
  pwindow,
  swindow,
  growth_rate,
  accrual = 0L
) {
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
    dist, args, cutoff, pwindow, swindow, growth_rate, accrual
  )))
  return(grid_cdf[cell + 1] * (1 - frac) + grid_cdf[cell + 2] * frac)
}

#' The distribution function of a continuous estimand under an accrual design
#'
#' Builds the accrual weighted distribution function on the quadrature grid and
#' interpolates it linearly at the reported value. The weight is offset by half
#' a primary window for the uniform single interval approximation, matching
#' [.meta_pcens_trunc_moments()], so that the reported quantile and the
#' reported moments describe the same estimand.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A probability.
#'
#' @keywords internal
.meta_accrual_prob <- function(
  y,
  dist,
  args,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate
) {
  if (y >= cutoff) {
    return(1)
  }
  n_quad <- .meta_n_quad()
  quad <- seq(0, cutoff, length.out = n_quad + 1)
  if (cens_adjusted == 2) {
    cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
    weight_offset <- pwindow / 2
  } else {
    cdf <- do.call(.meta_pdist(dist), c(list(q = quad), args))
    weight_offset <- 0
  }
  weighted <- .meta_accrual_reweight(cdf, cutoff, growth_rate, weight_offset)
  position <- y / cutoff * n_quad
  lower <- floor(position)
  frac <- position - lower
  return(weighted[lower + 1] * (1 - frac) + weighted[lower + 2] * frac)
}

#' The cumulative probability a study using a given procedure would report
#'
#' Evaluates the distribution function of the biased estimand at a reported
#' quantile value. Working on the probability scale avoids inverting the
#' distribution function, which has no closed form on the discrete grid.
#'
#' For a naive study (`cens_adjusted` of 0) the estimand is discrete, so the
#' continuity corrected grid distribution function of [.meta_grid_prob()] is
#' used. Midpoint imputation (`cens_adjusted` of 3) uses the same function
#' evaluated half a secondary window lower, because the study shifted every
#' delay up by that amount.
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
  growth_rate,
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 0 || cens_adjusted == 3) {
    shift <- ifelse(cens_adjusted == 3, swindow / 2, 0)
    return(.meta_grid_prob(
      y - shift, dist, args, cutoff, pwindow, swindow, growth_rate, accrual
    ))
  }
  if (y <= 0) {
    return(0)
  }
  if (accrual == 1L) {
    return(.meta_accrual_prob(
      y, dist, args, cutoff, pwindow, cens_adjusted, growth_rate
    ))
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

#' The density of the biased estimand at a reported quantile value
#'
#' Used to convert a quantile standard error reported on the delay scale to the
#' cumulative probability scale the model works on, by the delta method.
#' For a discrete estimand the density is the mass of the grid cell the value
#' falls in divided by the grid spacing, which is exactly the slope of the
#' continuity corrected distribution function there. For a continuous estimand
#' it is a central difference of the implied distribution function, which keeps
#' the same code path for every censoring adjustment and truncation design.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A density on the delay scale.
#'
#' @keywords internal
.meta_implied_density <- function(
  y,
  dist,
  args,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 0 || cens_adjusted == 3) {
    shift <- ifelse(cens_adjusted == 3, swindow / 2, 0)
    n_grid <- floor(cutoff / swindow)
    cell <- floor((y - shift) / swindow + 0.5)
    if (cell < 0 || cell >= n_grid) {
      return(0)
    }
    mass <- .meta_grid_pmf(
      dist, args, cutoff, pwindow, swindow, growth_rate, accrual
    )
    return(mass[cell + 1] / swindow)
  }
  half_width <- max(1e-6, 1e-4 * y)
  lower <- max(y - half_width, 0)
  upper <- y + half_width
  prob_upper <- .meta_implied_prob(
    upper, dist, args, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design
  )
  prob_lower <- .meta_implied_prob(
    lower, dist, args, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design
  )
  return(max((prob_upper - prob_lower) / (upper - lower), 0))
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
    trunc_design = prep$data$vint5[i],
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
#' A standard error reported for a quantile row is on the scale of the reported
#' delay, as studies report it, so it is converted to the cumulative
#' probability scale by the delta method using [.meta_implied_density()].
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
      slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate,
      slots$trunc_design
    )
    observed <- slots$quantile_p
    if (slots$report_se > 0) {
      implied_density <- .meta_implied_density(
        slots$value, dist, args, slots$cutoff, slots$pwindow, slots$swindow,
        slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate,
        slots$trunc_design
      )
      se <- max(implied_density * slots$report_se, .meta_min_prob_se())
    } else {
      se <- sqrt(slots$quantile_p * (1 - slots$quantile_p) / slots$study_n)
    }
  } else {
    moments <- .meta_implied_moments(
      dist, args, slots$cutoff, slots$pwindow, slots$swindow,
      slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate,
      slots$trunc_design
    )
    observed <- slots$value
    if (slots$obs_type == 2L) {
      implied <- moments[["mean"]]
      se <- moments[["sd"]] / sqrt(slots$study_n)
    } else {
      implied <- moments[["sd"]]
      se <- .meta_sd_se(moments, slots$study_n)
    }
    if (slots$report_se > 0) {
      se <- slots$report_se
    }
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
