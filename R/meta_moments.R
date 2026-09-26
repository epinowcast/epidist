#' Summarise a distribution from its mean and central moments
#'
#' The kurtosis is needed as well as the mean and standard deviation because
#' the sampling error of a reported standard deviation depends on it. See
#' [.meta_summary_terms()]. The skewness is needed because the sampling
#' covariance of a reported mean and a reported standard deviation from the
#' same study depends on it. See [.meta_moment_pair_ll()].
#'
#' @param mean The mean of the distribution.
#'
#' @param variance The variance of the distribution.
#'
#' @param third The third central moment of the distribution.
#'
#' @param fourth The fourth central moment of the distribution.
#'
#' @returns A named numeric vector with elements `mean`, `sd`, `kurtosis` and
#'  `skewness`.
#'
#' @keywords internal
.meta_moment_vector <- function(mean, variance, third, fourth) {
  variance <- max(variance, 1e-10)
  return(c(
    mean = mean,
    sd = sqrt(variance),
    kurtosis = max(fourth / variance^2, 1),
    skewness = third / variance^1.5
  ))
}

#' The summary vector returned when a normaliser underflows to zero
#'
#' An infinite mean and standard deviation make the normal log likelihood
#' evaluate to `-Inf` for any finite reported value, so a draw that hits this
#' case is rejected rather than turning the log likelihood into `NaN`.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_moment_failure <- function() {
  return(c(mean = Inf, sd = Inf, kurtosis = Inf, skewness = 0))
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
  third <- moments[3] - 3 * m1 * moments[2] + 2 * m1^3
  fourth <- moments[4] -
    4 * m1 * moments[3] +
    6 * m1^2 * moments[2] -
    3 * m1^4
  return(.meta_moment_vector(m1, variance, third, fourth))
}

#' Summaries of a discrete delay grid
#'
#' @param mass A vector of grid probabilities from [.meta_grid_pmf()], or a
#'  vector of `NA` if the grid mass underflowed to zero.
#'
#' @param first_delay The delay the first kept cell records.
#'
#' @param swindow The secondary censoring window width, which is also the grid
#'  spacing.
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_grid_moments <- function(mass, first_delay, swindow) {
  if (anyNA(mass)) {
    return(.meta_moment_failure())
  }
  delay <- first_delay + (seq_along(mass) - 1) * swindow
  m1 <- sum(mass * delay)
  centred <- delay - m1
  variance <- sum(mass * centred^2)
  third <- sum(mass * centred^3)
  fourth <- sum(mass * centred^4)
  return(.meta_moment_vector(m1, variance, third, fourth))
}

#' Analytic summaries of a delay distribution
#'
#' The mean and standard deviation mirror the formulas used by
#' [add_summaries()]. A draw wide enough to overflow a moment, such as a
#' lognormal whose `exp(4 * sdlog^2)` is infinite, is returned as
#' [.meta_moment_failure()] so that the row is rejected rather than carrying
#' a `NaN` into its standard error. Matches the reject in
#' `meta_family_moments()` in `inst/stan/meta_model/functions.stan`, where an
#' infinite intermediate would otherwise leave a finite density with a non
#' finite gradient.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_continuous_moments <- function(dist, args) {
  central <- switch(dist,
    plnorm = .meta_moments_lognormal(args),
    pgamma = .meta_moments_gamma(args),
    pweibull = .meta_moments_weibull(args),
    pgengamma.orig = .meta_moments_gengamma(args),
    pdiscretehazard = .meta_moments_np(args),
    cli::cli_abort(
      "Summary estimates are not supported for the {.val {dist}} distribution."
    )
  )
  moments <- do.call(.meta_moment_vector, as.list(unname(central)))
  if (!all(is.finite(moments))) {
    return(.meta_moment_failure())
  }
  return(moments)
}

#' Central moments of the delay distributions with analytic summaries
#'
#' Each returns the mean, the variance and the third and fourth central
#' moments. They mirror the Stan functions of the same name in
#' `inst/stan/meta_model/functions.stan`. The weibull and generalised gamma
#' share [.meta_moments_scaled()], since their raw moments are
#' \eqn{E[T^r] = scale^r g_r}.
#'
#' @param args A named list of distribution parameters.
#'
#' @param scale The scale of the distribution.
#'
#' @param g The ratios \eqn{g_1, \dots, g_4} of the raw moments.
#'
#' @returns A numeric vector of length four.
#'
#' @keywords internal
.meta_moments_lognormal <- function(args) {
  var_log <- args$sdlog^2
  delay_mean <- exp(args$meanlog + var_log / 2)
  variance <- delay_mean^2 * expm1(var_log)
  return(c(
    delay_mean,
    variance,
    (exp(var_log) + 2) * sqrt(expm1(var_log)) * variance^1.5,
    (exp(4 * var_log) + 2 * exp(3 * var_log) + 3 * exp(2 * var_log) - 3) *
      variance^2
  ))
}

#' @rdname dot-meta_moments_lognormal
#' @keywords internal
.meta_moments_gamma <- function(args) {
  variance <- args$shape * args$scale^2
  return(c(
    args$shape * args$scale,
    variance,
    2 / sqrt(args$shape) * variance^1.5,
    (3 + 6 / args$shape) * variance^2
  ))
}

#' @rdname dot-meta_moments_lognormal
#' @keywords internal
.meta_moments_scaled <- function(scale, g) {
  return(c(
    scale * g[1],
    scale^2 * (g[2] - g[1]^2),
    scale^3 * (g[3] - 3 * g[1] * g[2] + 2 * g[1]^3),
    scale^4 * (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] - 3 * g[1]^4)
  ))
}

#' @rdname dot-meta_moments_lognormal
#' @keywords internal
.meta_moments_weibull <- function(args) {
  return(.meta_moments_scaled(args$scale, gamma(1 + seq_len(4) / args$shape)))
}

#' @rdname dot-meta_moments_lognormal
#' @keywords internal
.meta_moments_gengamma <- function(args) {
  g <- exp(lgamma(args$k + seq_len(4) / args$shape) - lgamma(args$k))
  return(.meta_moments_scaled(args$scale, g))
}

#' @rdname dot-meta_moments_lognormal
#' @keywords internal
.meta_moments_np <- function(args) {
  # Point masses at the right edge of each bin
  edges <- args$boundaries[-1]
  mass <- primarycensored::hazards_to_pmf(args$hazards)
  delay_mean <- sum(mass * edges)
  centred <- edges - delay_mean
  return(c(
    delay_mean, sum(mass * centred^2), sum(mass * centred^3),
    sum(mass * centred^4)
  ))
}

#' Summaries implied by a distribution function evaluated on a grid
#'
#' Uses Simpson's rule on the survival integrals
#' \eqn{\int_L^D k t^{k - 1} (F(D) - F(t)) dt}, which with the boundary term
#' \eqn{L^k (F(D) - F(L))} give the first four raw moments of the distribution
#' truncated to \eqn{(L, D]}. The boundary term vanishes when \eqn{L} is zero,
#' recovering the untruncated expression. Matches the implementation in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param cdf The distribution function at `n_quad + 1` equally spaced points
#'  running from `lower` to `cutoff`, for an even `n_quad`.
#'
#' @param lower The study's minimum delay (its left truncation point).
#'
#' @param cutoff The right truncation point.
#'
#' @returns A named numeric vector with elements `mean`, `sd` and `kurtosis`,
#'  or [.meta_moment_failure()] if the distribution function holds no mass
#'  between `lower` and `cutoff`.
#'
#' @keywords internal
.meta_survival_moments <- function(cdf, lower = 0, cutoff) {
  n_quad <- length(cdf) - 1
  denom <- cdf[n_quad + 1] - cdf[1]
  if (!is.finite(denom) || denom <= 0) {
    return(.meta_moment_failure())
  }
  quad <- seq(lower, cutoff, length.out = n_quad + 1)
  weight <- c(1, rep_len(c(4, 2), n_quad - 1), 1)
  tail_prob <- cdf[n_quad + 1] - cdf
  raw_moments <- vapply(
    seq_len(4),
    function(k) {
      integrand <- k * quad^(k - 1) * tail_prob
      integral <- sum(weight * integrand) * (cutoff - lower) / (3 * n_quad)
      return(lower^k + integral / denom)
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
#' @param n_quad The number of quadrature intervals, an even number. Rows
#'  built by [as_epidist_meta_model()] carry the number chosen for their
#'  study by [.estimates_n_quad()].
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_trunc_moments <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  growth_rate = 0,
  accrual = 0L,
  n_quad = .meta_n_quad()
) {
  quad <- seq(lower, cutoff, length.out = n_quad + 1)
  cdf <- .meta_dist_cdf(quad, dist, args)
  if (accrual == 1L) {
    cdf <- .meta_accrual_reweight(cdf, lower, cutoff, growth_rate)
  }
  return(.meta_survival_moments(cdf, lower, cutoff))
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
#' @inheritParams .meta_trunc_moments
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_pcens_trunc_moments <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  growth_rate,
  accrual = 0L,
  n_quad = .meta_n_quad()
) {
  quad <- seq(lower, cutoff, length.out = n_quad + 1)
  cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
  if (accrual == 1L) {
    cdf <- .meta_accrual_reweight(cdf, lower, cutoff, growth_rate, pwindow / 2)
  }
  return(.meta_survival_moments(cdf, lower, cutoff))
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
  third_delay <- moments[["skewness"]] * var_delay^1.5
  fourth_delay <- moments[["kurtosis"]] * var_delay^2
  var_window <- pwindow^2 / 12
  fourth_window <- pwindow^4 / 80
  return(.meta_moment_vector(
    moments[["mean"]] + pwindow / 2,
    var_delay + var_window,
    third_delay,
    fourth_delay + 6 * var_delay * var_window + fourth_window
  ))
}

#' The first four raw moments of a set of summaries
#'
#' Inverts [.meta_central_from_raw()].
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @returns A numeric vector of the first four raw moments.
#'
#' @keywords internal
.meta_raw_from_central <- function(moments) {
  m1 <- moments[["mean"]]
  variance <- moments[["sd"]]^2
  third <- moments[["skewness"]] * variance^1.5
  fourth <- moments[["kurtosis"]] * variance^2
  return(c(
    m1,
    variance + m1^2,
    third + 3 * m1 * variance + m1^3,
    fourth + 4 * m1 * third + 6 * m1^2 * variance + m1^4
  ))
}

#' Summaries of a distribution left truncated at `lower`
#'
#' A study that adjusted for right truncation and only counted delays above
#' `lower` reported the moments of the delay conditioned on exceeding it. They
#' are taken from the untruncated moments by removing the part below `lower`,
#' \eqn{E[\tau^k 1(\tau \le L)] = L^k F(L) - \int_0^L k t^{k - 1} F(t) dt},
#' by Simpson's rule on \eqn{[0, L]}, and dividing by \eqn{1 - F(L)}. The
#' integral is over a bounded interval the quadrature resolves well, so the
#' result does not depend on `max_delay`, and it matches the distribution
#' function \eqn{(F(y) - F(L)) / (1 - F(L))} used for the same study's
#' quantile rows. It applies to any distribution whose untruncated moments and
#' distribution function are available, which is the delay itself and the
#' uniform single interval approximation of [.meta_add_uniform()].
#' Matches `meta_family_left_moments` in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param full A summary vector from [.meta_moment_vector()] of the
#'  untruncated distribution.
#'
#' @param cdf The distribution function at `n_quad + 1` equally spaced points
#'  running from zero to `lower`, for an even `n_quad`.
#'
#' @param lower The study's minimum delay (its left truncation point).
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_left_moments <- function(full, cdf, lower) {
  n_quad <- length(cdf) - 1
  tail_mass <- 1 - cdf[n_quad + 1]
  if (!is.finite(tail_mass) || tail_mass <= 0) {
    return(.meta_moment_failure())
  }
  quad <- seq(0, lower, length.out = n_quad + 1)
  weight <- c(1, rep_len(c(4, 2), n_quad - 1), 1)
  below <- vapply(
    seq_len(4),
    function(k) {
      integral <- sum(weight * k * quad^(k - 1) * cdf) * lower / (3 * n_quad)
      return(lower^k * cdf[n_quad + 1] - integral)
    },
    numeric(1)
  )
  return(
    .meta_central_from_raw((.meta_raw_from_central(full) - below) / tail_mass)
  )
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
#' A study that adjusted for right truncation and counted only delays above
#' `lower` has its analytic moments left truncated by [.meta_left_moments()],
#' so they do not depend on `cutoff`. A study whose primary events were not
#' uniform within their window has no analytic moments, so it is truncated
#' at `cutoff` by quadrature instead.
#'
#' Under midpoint imputation (`cens_adjusted` of 3) the study assigned each
#' delay to the centre of the interval it was observed in, so the estimand is
#' the naive discrete grid shifted up by `swindow / 2`. The shift moves the
#' mean and leaves every central moment unchanged.
#'
#' Under midpoint imputation with a uniform interval (`cens_adjusted` of 4) the
#' study placed the primary event at the midpoint of its window instead of at
#' its lower edge, so the estimand is that of `cens_adjusted` of 2 shifted down
#' by `pwindow / 2`. Both midpoint codes are evaluated by calling the code they
#' shift. See [.meta_cens_base()] and [.meta_cens_shift()].
#'
#' @inheritParams .meta_grid_pmf
#'
#' @param trunc_adjusted 1 if the study adjusted for right truncation, 0
#'  otherwise.
#'
#' @param cens_adjusted The censoring adjustment code, one of 0, 1, 2, 3, or
#'  4.
#'
#' @param trunc_design 0 for a cohort design, 1 for an accrual design.
#'
#' @inheritParams .meta_trunc_moments
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_implied_moments <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L,
  n_quad = .meta_n_quad()
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    # Midpoint imputation moves the base estimand along the delay axis, so its
    # mean and its left truncation point move and every central moment is
    # unchanged. The cutoff stays where it is, because the observation time
    # bounds the underlying event rather than the midpointed value.
    moments <- .meta_implied_moments(
      dist, args, .meta_cens_lower(lower, cens_adjusted, pwindow, swindow),
      cutoff, pwindow, swindow, trunc_adjusted,
      .meta_cens_base(cens_adjusted), growth_rate, trunc_design, n_quad
    )
    moments[["mean"]] <- moments[["mean"]] +
      .meta_cens_shift(cens_adjusted, pwindow, swindow)
    return(moments)
  }
  if (cens_adjusted == 0) {
    mass <- .meta_grid_pmf(
      dist, args, lower, cutoff, pwindow, swindow, growth_rate, accrual
    )
    first <- .meta_grid_first(lower, swindow)
    return(.meta_grid_moments(mass, first * swindow, swindow))
  }
  if (cens_adjusted == 2) {
    if (trunc_adjusted == 1 && growth_rate == 0) {
      full <- .meta_add_uniform(.meta_continuous_moments(dist, args), pwindow)
      if (lower == 0) {
        return(full)
      }
      quad <- seq(0, lower, length.out = n_quad + 1)
      return(.meta_left_moments(
        full, .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate), lower
      ))
    }
    return(.meta_pcens_trunc_moments(
      dist, args, lower, cutoff, pwindow, growth_rate, accrual, n_quad
    ))
  }
  if (trunc_adjusted == 1) {
    full <- .meta_continuous_moments(dist, args)
    if (lower == 0) {
      return(full)
    }
    quad <- seq(0, lower, length.out = n_quad + 1)
    return(.meta_left_moments(
      full, .meta_dist_cdf(quad, dist, args), lower
    ))
  }
  return(.meta_trunc_moments(
    dist, args, lower, cutoff, growth_rate, accrual, n_quad
  ))
}
