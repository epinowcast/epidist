#' The default number of quadrature intervals for truncated continuous moments
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad_default <- function() {
  return(100L)
}

#' The number of quadrature intervals used for truncated continuous moments
#'
#' Set with `options(epidist.meta_n_quad = )`, as an even number of at least
#' two. Set it before fitting, since it is compiled into the model.
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad <- function() {
  n_quad <- getOption("epidist.meta_n_quad", .meta_n_quad_default())
  assert_integerish(
    n_quad,
    lower = 2, len = 1, any.missing = FALSE,
    .var.name = "options(epidist.meta_n_quad)"
  )
  if (n_quad %% 2 != 0) {
    cli::cli_abort(paste0(
      "{.code options(epidist.meta_n_quad)} must be an even number of ",
      "intervals, because the quadrature uses Simpson's rule."
    ))
  }
  return(as.integer(n_quad))
}

#' Implied summaries shared by meta model rows with the same study design
#'
#' Holds one entry per study design, each a list of the parameter draws it was
#' built from and the summaries they imply. It lives in the package namespace,
#' so it is never written into a fitted model object.
#' See [.meta_row_draw_moments()].
#'
#' @format An environment.
#'
#' @keywords internal
.meta_draws <- new.env(parent = emptyenv())

#' The largest number of entries the implied summary cache holds
#'
#' The cache is bounded so that it cannot grow without limit over a long
#' session. Passing the limit clears it rather than evicting one entry, which
#' keeps the bookkeeping to a single check. Each entry holds one summary vector
#' per posterior draw, so the limit is small.
#'
#' @returns An integer number of entries.
#'
#' @keywords internal
.meta_draw_cache_limit <- function() {
  return(8L)
}

#' The density function used for a `primarycensored` distribution name
#'
#' Shares the distribution function lookup with [.pdist()] in `R/gen.R`; only
#' the density direction is meta model specific.
#'
#' @inheritParams .pdist
#'
#' @returns The corresponding function from `stats`.
#'
#' @keywords internal
.meta_ddist <- function(dist) {
  return(switch(dist,
    plnorm = stats::dlnorm,
    pgamma = stats::dgamma,
    pweibull = stats::dweibull,
    get(sub("^p", "d", dist), envir = asNamespace("stats"))
  ))
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
        pdist = .pdist(dist),
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

#' The censoring adjustment whose estimand a code is built on
#'
#' Two adjustment codes are another code's estimand moved along the delay axis
#' by a fixed amount, because the study replaced an event time with the
#' midpoint of its window. Midpoint imputation of the secondary interval
#' (code 3) moves the naive discrete grid of code 0. Midpoint imputation of the
#' primary event (code 4) moves the primary censored estimand of code 2. Both
#' are evaluated by calling the base code and moving the result, so each
#' estimand is implemented once.
#'
#' Matches `meta_family_cens_base` in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param cens_adjusted The censoring adjustment code, one of 0, 1, 2, 3, or 4.
#'
#' @returns The code whose estimand is evaluated.
#'
#' @keywords internal
.meta_cens_base <- function(cens_adjusted) {
  if (cens_adjusted == 3) {
    return(0L)
  }
  if (cens_adjusted == 4) {
    return(2L)
  }
  return(as.integer(cens_adjusted))
}

#' The delay a midpoint imputation moves the base estimand by
#'
#' Midpoint imputation of the secondary interval (code 3) assigns each delay to
#' the centre of the interval it was seen in, moving it up by half a secondary
#' window. Midpoint imputation of the primary event (code 4) anchors the delay
#' at the centre of the primary window rather than at its lower edge, moving it
#' down by half a primary window. Every other code leaves its estimand where it
#' is.
#'
#' Matches `meta_family_shift` in `inst/stan/meta_model/functions.stan`.
#'
#' @inheritParams .meta_cens_base
#'
#' @param pwindow,swindow The primary and secondary censoring window widths.
#'
#' @returns A signed delay.
#'
#' @keywords internal
.meta_cens_shift <- function(cens_adjusted, pwindow, swindow) {
  if (cens_adjusted == 3) {
    return(swindow / 2)
  }
  if (cens_adjusted == 4) {
    return(-pwindow / 2)
  }
  return(0)
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
#' returned relative to their largest value to avoid overflow. A growth rate of
#' zero makes the follow up linear in the delay, which is taken directly rather
#' than through the log scale form.
#'
#' @inheritParams .meta_log_accrual_weight
#'
#' @returns A numeric vector of relative weights with a maximum of one.
#'
#' @keywords internal
.meta_accrual_weight <- function(d, window, growth_rate) {
  if (growth_rate == 0) {
    remaining <- pmax(window - d, 0)
    peak <- max(remaining)
    if (peak <= 0) {
      return(remaining)
    }
    return(remaining / peak)
  }
  log_weight <- .meta_log_accrual_weight(d, window, growth_rate)
  return(exp(log_weight - max(log_weight)))
}

#' The index of the first grid cell a left truncated study could have seen
#'
#' A study that only counted delays of at least `lower` never saw the grid
#' cells recording a shorter delay, so they are dropped before the grid is
#' renormalised. The index counts cells from zero, so it is zero when the
#' study counted every delay.
#'
#' @param lower The study's minimum delay (its left truncation point).
#'
#' @param swindow The secondary censoring window width, which is also the grid
#'  spacing.
#'
#' @returns An integer cell index.
#'
#' @keywords internal
.meta_grid_first <- function(lower, swindow) {
  return(as.integer(ceiling(lower / swindow - 1e-9)))
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
#'  `lower` to `cutoff`.
#'
#' @param lower The study's minimum delay (its left truncation point).
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
  lower,
  cutoff,
  growth_rate,
  weight_offset = 0
) {
  n_quad <- length(cdf) - 1
  span <- cutoff - lower
  midpoint <- lower + (seq_len(n_quad) - 0.5) * span / n_quad - weight_offset
  mass <- pmax(diff(cdf), 0) *
    .meta_accrual_weight(midpoint, cutoff, growth_rate)
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
#' Cells recording a delay below `lower` are dropped before the grid is
#' renormalised, which conditions it on the study's left truncation point. The
#' normaliser is then the mass of the kept cells, which is
#' \eqn{F(D) - F(L)} whenever `lower` falls on a grid boundary.
#'
#' Under an accrual design the cell masses are additionally weighted by the
#' follow up available to the delay each cell records, which is the delay at
#' its lower edge, before renormalising.
#'
#' A cohort grid is normalised by the distribution function at its top, which
#' is already known. An accrual grid reweights each cell first, so its
#' normaliser is not known in advance.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param lower The study's minimum delay (its left truncation point).
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
#' @returns A numeric vector of probabilities summing to one, one per kept
#'  cell, or a vector of `NA` the same length if the grid mass underflows to
#'  zero.
#'
#' @keywords internal
.meta_grid_pmf <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate,
  accrual = 0L
) {
  n_grid <- floor(cutoff / swindow)
  first <- .meta_grid_first(lower, swindow)
  if (n_grid - first < 1) {
    cli::cli_abort(paste0(
      "The grid of a study with a {.var delay_min} of {.val {lower}} holds ",
      "no cells below its cutoff."
    ))
  }
  boundary <- seq(first, n_grid) * swindow
  cdf <- .meta_pcens_cdf(boundary, dist, args, pwindow, growth_rate)
  # Once the distribution function saturates its differences can come back
  # very slightly negative, which would leave an invalid pmf. Stan builds the
  # same cells on the log scale and drops them to zero there.
  mass <- pmax(diff(cdf), 0)
  if (accrual != 1L) {
    total <- cdf[length(cdf)] - cdf[1]
    if (!is.finite(total) || total <= 0) {
      return(rep(NA_real_, length(mass)))
    }
    return(mass / total)
  }
  mass <- mass *
    .meta_accrual_weight(boundary[-length(boundary)], cutoff, growth_rate)
  total <- sum(mass)
  if (!is.finite(total) || total <= 0) {
    return(rep(NA_real_, length(mass)))
  }
  return(mass / total)
}

#' The cohort grid distribution function at the two edges of one cell
#'
#' A cohort grid is normalised by the mass it holds, so its cumulative sum at
#' cell \eqn{k} is
#' \eqn{(F(k \times swindow) - F(L)) / (F(n_{grid} \times swindow) - F(L))}.
#' A reported quantile therefore needs four distribution function evaluations
#' rather than the whole grid, and three where the study counted every delay.
#' This does not hold under an accrual design, which reweights each cell
#' before renormalising and so must keep the full grid.
#'
#' @param cell The index of the grid cell, counting from zero.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns The grid distribution function at the lower and upper cell edges,
#'  or two infinities if the grid mass underflows to zero.
#'
#' @keywords internal
.meta_grid_edges <- function(
  cell,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate
) {
  n_grid <- floor(cutoff / swindow)
  first <- .meta_grid_first(lower, swindow)
  edges <- c(cell, min(cell + 1, n_grid), n_grid, first)
  cdf <- .meta_pcens_cdf(edges * swindow, dist, args, pwindow, growth_rate)
  total <- cdf[3] - cdf[4]
  if (!is.finite(total) || total <= 0) {
    return(c(Inf, Inf))
  }
  return((cdf[1:2] - cdf[4]) / total)
}

#' The continuity corrected cohort grid distribution function at several delays
#'
#' The vectorised form of [.meta_grid_prob()] for a cohort study. Every cell
#' edge a set of reported quantiles needs is evaluated in one call, because a
#' call to [primarycensored::pprimarycensored()] costs the same whether it is
#' given one delay or a hundred.
#'
#' @param y A numeric vector of delays.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns A numeric vector of probabilities, or infinities if the grid mass
#'  underflows to zero.
#'
#' @keywords internal
.meta_grid_probs <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate
) {
  n_grid <- floor(cutoff / swindow)
  first <- .meta_grid_first(lower, swindow)
  cell <- floor(y / swindow + 0.5)
  frac <- y / swindow + 0.5 - cell
  inside <- cell >= first & cell < n_grid
  edges <- unique(c(
    pmin(pmax(c(cell, cell + 1), first), n_grid), n_grid, first
  ))
  cdf <- .meta_pcens_cdf(edges * swindow, dist, args, pwindow, growth_rate)
  base <- cdf[match(first, edges)]
  total <- cdf[match(n_grid, edges)] - base
  if (!is.finite(total) || total <= 0) {
    return(rep(Inf, length(y)))
  }
  scaled <- (cdf - base) / total
  prob <- scaled[match(cell, edges)] * (1 - frac) +
    scaled[match(cell + 1, edges)] * frac
  prob[!inside] <- as.numeric(cell[!inside] >= n_grid)
  return(prob)
}

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
    skewness <- (exp(var_log) + 2) * sqrt(expm1(var_log))
    return(.meta_moment_vector(
      delay_mean, variance, skewness * variance^1.5, kurtosis * variance^2
    ))
  }
  if (identical(dist, "pgamma")) {
    variance <- args$shape * args$scale^2
    return(.meta_moment_vector(
      args$shape * args$scale,
      variance,
      2 / sqrt(args$shape) * variance^1.5,
      (3 + 6 / args$shape) * variance^2
    ))
  }
  if (identical(dist, "pweibull")) {
    g <- gamma(1 + seq_len(4) / args$shape)
    variance <- args$scale^2 * (g[2] - g[1]^2)
    third <- args$scale^3 * (g[3] - 3 * g[1] * g[2] + 2 * g[1]^3)
    fourth <- args$scale^4 *
      (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] - 3 * g[1]^4)
    return(.meta_moment_vector(args$scale * g[1], variance, third, fourth))
  }
  return(cli::cli_abort(
    "Summary estimates are not supported for the {.val {dist}} distribution."
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
#' @param cdf The distribution function at `.meta_n_quad() + 1` equally spaced
#'  points running from `lower` to `cutoff`.
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
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_trunc_moments <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  growth_rate = 0,
  accrual = 0L
) {
  quad <- seq(lower, cutoff, length.out = .meta_n_quad() + 1)
  cdf <- do.call(.pdist(dist), c(list(q = quad), args))
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
  accrual = 0L
) {
  quad <- seq(lower, cutoff, length.out = .meta_n_quad() + 1)
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
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    # Midpoint imputation moves the base estimand along the delay axis, so its
    # mean moves and every central moment is unchanged.
    moments <- .meta_implied_moments(
      dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
      .meta_cens_base(cens_adjusted), growth_rate, trunc_design
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
    if (trunc_adjusted == 1 && growth_rate == 0 && lower == 0) {
      return(.meta_add_uniform(.meta_continuous_moments(dist, args), pwindow))
    }
    return(.meta_pcens_trunc_moments(
      dist, args, lower, cutoff, pwindow, growth_rate, accrual
    ))
  }
  if (trunc_adjusted == 1 && lower == 0) {
    return(.meta_continuous_moments(dist, args))
  }
  return(.meta_trunc_moments(dist, args, lower, cutoff, growth_rate, accrual))
}

#' The continuity corrected distribution function of a discrete delay grid
#'
#' The step distribution function of the grid is replaced by the version that
#' interpolates it linearly through the mid points of its cells. Without this
#' correction a quantile of day resolution data, which must land on a jump of
#' the step function, biases the implied probability upwards by several
#' sampling standard errors.
#'
#' A cohort grid only needs the two cell edges the value falls between, so it
#' takes the three evaluation shortcut of [.meta_grid_edges()]. An accrual grid
#' must be built in full.
#'
#' @param y The delay to evaluate the distribution function at.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns A probability, or `Inf` if the grid mass underflows to zero,
#'  which forces a `-Inf` log likelihood rather than a `NaN` one.
#'
#' @keywords internal
.meta_grid_prob <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate,
  accrual = 0L
) {
  n_grid <- floor(cutoff / swindow)
  first <- .meta_grid_first(lower, swindow)
  cell <- floor(y / swindow + 0.5)
  frac <- y / swindow + 0.5 - cell
  if (cell < first) {
    return(0)
  }
  if (cell >= n_grid) {
    return(1)
  }
  if (accrual != 1L) {
    edges <- .meta_grid_edges(
      cell, dist, args, lower, cutoff, pwindow, swindow, growth_rate
    )
    if (!all(is.finite(edges))) {
      return(Inf)
    }
    return(edges[1] * (1 - frac) + edges[2] * frac)
  }
  mass <- .meta_grid_pmf(
    dist, args, lower, cutoff, pwindow, swindow, growth_rate, accrual
  )
  if (anyNA(mass)) {
    return(Inf)
  }
  grid_cdf <- c(0, cumsum(mass))
  return(
    grid_cdf[cell - first + 1] * (1 - frac) +
      grid_cdf[cell - first + 2] * frac
  )
}

#' The accrual weighted distribution function on the quadrature grid
#'
#' The weight is offset by half a primary window for the uniform single
#' interval approximation, matching [.meta_pcens_trunc_moments()], so that the
#' reported quantile and the reported moments describe the same estimand.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A distribution function at `.meta_n_quad() + 1` equally spaced
#'  nodes.
#'
#' @keywords internal
.meta_accrual_nodes <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate
) {
  quad <- seq(lower, cutoff, length.out = .meta_n_quad() + 1)
  if (cens_adjusted == 2) {
    cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
    weight_offset <- pwindow / 2
  } else {
    cdf <- do.call(.pdist(dist), c(list(q = quad), args))
    weight_offset <- 0
  }
  return(
    .meta_accrual_reweight(cdf, lower, cutoff, growth_rate, weight_offset)
  )
}

#' The distribution function of a continuous estimand under an accrual design
#'
#' Interpolates the accrual weighted distribution function of
#' [.meta_accrual_nodes()] linearly at the reported value.
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
  lower = 0,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate
) {
  if (y >= cutoff) {
    return(1)
  }
  if (y <= lower) {
    return(0)
  }
  weighted <- .meta_accrual_nodes(
    dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate
  )
  position <- (y - lower) / (cutoff - lower) * .meta_n_quad()
  node <- floor(position)
  frac <- position - node
  return(weighted[node + 1] * (1 - frac) + weighted[node + 2] * frac)
}

#' The density of a continuous estimand under an accrual design
#'
#' The distribution function is interpolated linearly between quadrature nodes,
#' so its slope on the interval containing the reported value is the density
#' there.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A density on the delay scale.
#'
#' @keywords internal
.meta_accrual_density <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate
) {
  if (y >= cutoff || y <= lower) {
    return(0)
  }
  n_quad <- .meta_n_quad()
  span <- cutoff - lower
  weighted <- .meta_accrual_nodes(
    dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate
  )
  node <- floor((y - lower) / span * n_quad)
  slope <- (weighted[node + 2] - weighted[node + 1]) * n_quad / span
  return(max(slope, 0))
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
#' Midpoint imputation with a uniform interval (`cens_adjusted` of 4) uses that
#' function evaluated half a primary window higher, because the study anchored
#' every delay at the centre of the primary window rather than at its lower
#' edge.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A probability, or `Inf` if the distribution function underflows
#'  to zero at `cutoff`, which forces a `-Inf` log likelihood rather than a
#'  `NaN` one.
#'
#' @keywords internal
.meta_implied_prob <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    # Midpoint imputation moved every delay along the axis, so the base
    # estimand is evaluated at the reported delay moved back.
    return(.meta_implied_prob(
      y - .meta_cens_shift(cens_adjusted, pwindow, swindow), dist, args,
      lower, cutoff, pwindow, swindow, trunc_adjusted,
      .meta_cens_base(cens_adjusted), growth_rate, trunc_design
    ))
  }
  if (cens_adjusted == 0) {
    return(.meta_grid_prob(
      y, dist, args, lower, cutoff, pwindow, swindow, growth_rate, accrual
    ))
  }
  if (y <= lower) {
    return(0)
  }
  if (accrual == 1L) {
    return(.meta_accrual_prob(
      y, dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate
    ))
  }
  if (cens_adjusted == 2) {
    cdf <- function(q) {
      return(.meta_pcens_cdf(q, dist, args, pwindow, growth_rate))
    }
  } else {
    cdf <- function(q) {
      return(do.call(.pdist(dist), c(list(q = q), args)))
    }
  }
  base <- ifelse(lower > 0, cdf(lower), 0)
  if (trunc_adjusted == 1) {
    if (!is.finite(base) || base >= 1) {
      return(Inf)
    }
    return(min((cdf(y) - base) / (1 - base), 1))
  }
  if (y >= cutoff) {
    return(1)
  }
  denom <- cdf(cutoff) - base
  if (!is.finite(denom) || denom <= 0) {
    return(Inf)
  }
  return(min((cdf(y) - base) / denom, 1))
}

#' The density of a delay censored by a uniform primary window
#'
#' Averaging the distribution function over a uniform primary window makes the
#' density of the primary censored delay the difference of the delay
#' distribution function across the window, divided by its width.
#'
#' @param y The delay.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns A density on the delay scale.
#'
#' @keywords internal
.meta_uniform_pcens_density <- function(y, dist, args, pwindow) {
  pdist <- .pdist(dist)
  upper <- do.call(pdist, c(list(q = y), args))
  lower <- do.call(pdist, c(list(q = max(y - pwindow, 0)), args))
  return(max(upper - lower, 0) / pwindow)
}

#' A central difference of the implied distribution function
#'
#' Used where the density of the biased estimand has no closed form, which is
#' the primary censored delay of a study whose primary events were not uniform
#' within their window.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A density on the delay scale.
#'
#' @keywords internal
.meta_central_difference <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L
) {
  half_width <- max(1e-6, 1e-4 * y)
  step_lwr <- max(y - half_width, lower)
  step_upr <- y + half_width
  prob_upper <- .meta_implied_prob(
    step_upr, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design
  )
  prob_lower <- .meta_implied_prob(
    step_lwr, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design
  )
  return(max((prob_upper - prob_lower) / (step_upr - step_lwr), 0))
}

#' The density of the biased estimand at a reported quantile value
#'
#' Used to convert a quantile standard error reported on the delay scale to the
#' cumulative probability scale the model works on, by the delta method.
#' For a discrete estimand the density is the mass of the grid cell the value
#' falls in divided by the grid spacing, which is exactly the slope of the
#' continuity corrected distribution function there. For a continuous estimand
#' it is the closed form density of the estimand over its truncation
#' normaliser, or, under an accrual design, the slope of the interpolated
#' distribution function. A primary censored delay with a non uniform primary
#' event has no closed form density, so it falls back to a central difference.
#'
#' @param y The reported quantile value.
#'
#' @inheritParams .meta_implied_moments
#'
#' @returns A density on the delay scale, or `Inf` if the grid mass or the
#'  distribution function underflows to zero, which forces a `-Inf` log
#'  likelihood rather than a `NaN` one.
#'
#' @keywords internal
.meta_implied_density <- function(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    return(.meta_implied_density(
      y - .meta_cens_shift(cens_adjusted, pwindow, swindow), dist, args,
      lower, cutoff, pwindow, swindow, trunc_adjusted,
      .meta_cens_base(cens_adjusted), growth_rate, trunc_design
    ))
  }
  if (cens_adjusted == 0) {
    n_grid <- floor(cutoff / swindow)
    first <- .meta_grid_first(lower, swindow)
    cell <- floor(y / swindow + 0.5)
    if (cell < first || cell >= n_grid) {
      return(0)
    }
    if (accrual != 1L) {
      edges <- .meta_grid_edges(
        cell, dist, args, lower, cutoff, pwindow, swindow, growth_rate
      )
      if (!all(is.finite(edges))) {
        return(Inf)
      }
      return((edges[2] - edges[1]) / swindow)
    }
    mass <- .meta_grid_pmf(
      dist, args, lower, cutoff, pwindow, swindow, growth_rate, accrual
    )
    if (anyNA(mass)) {
      return(Inf)
    }
    return(mass[cell - first + 1] / swindow)
  }
  if (y <= lower || (trunc_adjusted != 1 && y >= cutoff)) {
    return(0)
  }
  if (accrual == 1L) {
    return(.meta_accrual_density(
      y, dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate
    ))
  }
  if (cens_adjusted == 2 && growth_rate != 0) {
    return(.meta_central_difference(
      y, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
      cens_adjusted, growth_rate, trunc_design
    ))
  }
  if (cens_adjusted == 2) {
    height <- .meta_uniform_pcens_density(y, dist, args, pwindow)
    base <- ifelse(
      lower > 0, .meta_pcens_cdf(lower, dist, args, pwindow, growth_rate), 0
    )
  } else {
    height <- do.call(.meta_ddist(dist), c(list(x = y), args))
    base <- ifelse(
      lower > 0, do.call(.pdist(dist), c(list(q = lower), args)), 0
    )
  }
  if (trunc_adjusted == 1) {
    denom <- 1 - base
  } else if (cens_adjusted == 2) {
    denom <- .meta_pcens_cdf(cutoff, dist, args, pwindow, growth_rate) - base
  } else {
    denom <- do.call(.pdist(dist), c(list(q = cutoff), args)) - base
  }
  if (!is.finite(denom) || denom <= 0) {
    return(Inf)
  }
  return(height / denom)
}

#' Extract the meta model slots for a single row
#'
#' Group rows point into the flat member arrays passed to Stan as data, so the
#' reported values and cumulative counts of the group are read back out here
#' for the R mirrors of the joint likelihoods.
#'
#' @param i The row index.
#'
#' @param prep A `brms` prep object.
#'
#' @returns A named list of the observation type, study metadata and reported
#'  values for row `i`.
#'
#' @keywords internal
.meta_row_slots <- function(i, prep) {
  group_start <- prep$data$vint6[i]
  group_len <- prep$data$vint7[i]
  chol_start <- prep$data$vint8[i]
  member <- seq_len(group_len) + group_start - 1L
  entry <- seq_len(group_len^2) + chol_start - 1L
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
    lower = prep$data$vreal5[i],
    report_se = prep$data$vreal6[i],
    quantile_p = prep$data$vreal7[i],
    growth_rate = prep$data$vreal8[i],
    group_value = as.numeric(prep$data$meta_group_value)[member],
    group_count = as.numeric(prep$data$meta_group_count)[member],
    group_type = as.integer(prep$data$meta_group_type)[member],
    group_p = as.numeric(prep$data$meta_group_p)[member],
    group_chol = matrix(
      as.numeric(prep$data$meta_group_chol)[entry], group_len, group_len
    )
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

#' The largest correlation the joint moment likelihood will use
#'
#' The asymptotic correlation between a sample mean and a sample standard
#' deviation is the skewness over the square root of the excess kurtosis, and
#' \eqn{\kappa \geq \gamma_1^2 + 1} holds for every distribution, so it never
#' leaves \eqn{[-1, 1]}. Moments taken from a discrete grid or from quadrature
#' can sit a little outside that bound, which would make the covariance matrix
#' singular, so the correlation is held inside it.
#'
#' Matches the value hard coded in `inst/stan/meta_model/functions.stan` so
#' that the R and Stan implementations agree.
#'
#' @returns A correlation.
#'
#' @keywords internal
.meta_max_correlation <- function() {
  return(1 - 1e-6)
}

#' The correlation between a reported mean and a reported standard deviation
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @returns A correlation strictly inside \eqn{[-1, 1]}.
#'
#' @keywords internal
.meta_moment_correlation <- function(moments) {
  excess <- max(moments[["kurtosis"]] - 1, 1e-10)
  rho <- moments[["skewness"]] / sqrt(excess)
  limit <- .meta_max_correlation()
  return(max(min(rho, limit), -limit))
}

#' The joint log likelihood of a mean and a standard deviation from one study
#'
#' A sample mean and a sample standard deviation computed from the same delays
#' are correlated, so fitting them as two independent normal terms overstates
#' how much a study reporting both tells us. They are instead given the
#' asymptotic bivariate normal of the pair, with
#' \eqn{\text{Cov}(\bar{x}, s^2) = \mu_3 / n} carried onto the standard
#' deviation scale by the delta method, giving
#' \eqn{\text{Cov}(\bar{x}, s) = \mu_3 / (2 n \sigma)} and a correlation of
#' \eqn{\gamma_1 / \sqrt{\kappa - 1}}. See [.meta_moment_correlation()].
#'
#' @param y_mean The reported mean.
#'
#' @param y_sd The reported standard deviation.
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @param study_n The number of delays the summaries were computed from.
#'
#' @returns A log density.
#'
#' @keywords internal
.meta_moment_pair_ll <- function(y_mean, y_sd, moments, study_n) {
  if (!all(is.finite(moments))) {
    return(-Inf)
  }
  se_mean <- moments[["sd"]] / sqrt(study_n)
  se_sd <- .meta_sd_se(moments, study_n)
  rho <- .meta_moment_correlation(moments)
  z_mean <- (y_mean - moments[["mean"]]) / se_mean
  z_sd <- (y_sd - moments[["sd"]]) / se_sd
  quadratic <- z_mean^2 - 2 * rho * z_mean * z_sd + z_sd^2
  return(
    -log(2 * pi) -
      log(se_mean) -
      log(se_sd) -
      0.5 * log1p(-rho^2) -
      quadratic / (2 * (1 - rho^2))
  )
}

#' The cumulative counts implied by a set of reported quantiles
#'
#' The multinomial likelihood of [.meta_quantile_set_ll()] needs the number of
#' delays a study saw at or below each reported quantile. Rounding the
#' cumulative probabilities rather than the increments keeps the counts
#' non decreasing and bounded by the sample size, so the cell counts are non
#' negative and sum to the sample size however the probabilities round.
#'
#' @param p A vector of quantile probabilities in increasing order.
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @returns An integer vector of cumulative counts.
#'
#' @keywords internal
.meta_quantile_counts <- function(p, study_n) {
  cumulative <- round(study_n * p)
  cumulative <- pmin(pmax(cumulative, 0), study_n)
  return(as.integer(cummax(cumulative)))
}

#' Interpolate a distribution function held at equally spaced points
#'
#' Both the discrete grid and the accrual quadrature give the implied
#' distribution function at equally spaced delays, and a reported quantile is
#' read off by linear interpolation between them. Delays below the first point
#' are given zero and delays at or beyond the last are given one.
#'
#' @param y A numeric vector of delays.
#'
#' @param values The distribution function at the points.
#'
#' @param spacing The distance between consecutive points.
#'
#' @param offset The offset, in points, of the first point from a delay of
#'  zero. This is half a cell for the continuity corrected grid, which
#'  interpolates through the mid points of its cells, and zero for the accrual
#'  quadrature.
#'
#' @returns A numeric vector of probabilities.
#'
#' @keywords internal
.meta_interpolate <- function(y, values, spacing, offset) {
  n_interval <- length(values) - 1
  position <- y / spacing + offset
  index <- floor(position)
  frac <- position - index
  inside <- index >= 0 & index < n_interval
  prob <- rep(0, length(y))
  prob[index >= n_interval] <- 1
  if (any(inside)) {
    lower <- index[inside]
    prob[inside] <- values[lower + 1] * (1 - frac[inside]) +
      values[lower + 2] * frac[inside]
  }
  return(prob)
}

#' The cumulative probabilities a study would report at several quantiles
#'
#' The vectorised form of [.meta_implied_prob()], used for the set of quantiles
#' one study reported. A cohort study on a discrete grid needs only the cell
#' edges the reported values fall between, so all of them are evaluated in one
#' call. Every other design falls back to evaluating each value on its own.
#'
#' @param y A vector of reported quantile values.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns A numeric vector of probabilities.
#'
#' @keywords internal
.meta_implied_probs <- function(y, dist, args, slots) {
  accrual <- .meta_accrual_flag(slots$trunc_adjusted, slots$trunc_design)
  first <- .meta_grid_first(slots$lower, slots$swindow)
  if (slots$cens_adjusted %in% c(3, 4)) {
    shifted <- slots
    shifted$cens_adjusted <- .meta_cens_base(slots$cens_adjusted)
    return(.meta_implied_probs(
      y - .meta_cens_shift(
        slots$cens_adjusted, slots$pwindow, slots$swindow
      ),
      dist, args, shifted
    ))
  }
  if (slots$cens_adjusted == 0) {
    if (accrual != 1L) {
      return(.meta_grid_probs(
        y, dist, args, slots$lower, slots$cutoff, slots$pwindow,
        slots$swindow, slots$growth_rate
      ))
    }
    mass <- .meta_grid_pmf(
      dist, args, slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
      slots$growth_rate, accrual
    )
    if (anyNA(mass)) {
      return(rep(Inf, length(y)))
    }
    return(.meta_interpolate(
      y - (first - 0.5) * slots$swindow, c(0, cumsum(mass)),
      slots$swindow, 0
    ))
  }
  if (accrual == 1L) {
    nodes <- .meta_accrual_nodes(
      dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$cens_adjusted, slots$growth_rate
    )
    return(.meta_interpolate(
      y - slots$lower, nodes,
      (slots$cutoff - slots$lower) / .meta_n_quad(), 0
    ))
  }
  return(vapply(
    y,
    .meta_implied_prob,
    numeric(1),
    dist = dist, args = args, lower = slots$lower, cutoff = slots$cutoff,
    pwindow = slots$pwindow, swindow = slots$swindow,
    trunc_adjusted = slots$trunc_adjusted,
    cens_adjusted = slots$cens_adjusted, growth_rate = slots$growth_rate,
    trunc_design = slots$trunc_design
  ))
}

#' The implied distribution function at equally spaced delays
#'
#' Returns the implied distribution function on a grid of delays, together
#' with where that grid starts and how far apart its points are. Reading an
#' implied quantile off the delay scale needs the inverse of the implied
#' distribution function, which has no closed form on the discrete grid, so it
#' is interpolated from these points instead. See [.meta_node_quantile()].
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns A list with the distribution function `values`, the delay
#'  `origin` of the first value, and the `spacing` between values.
#'
#' @keywords internal
.meta_implied_nodes <- function(dist, args, slots) {
  accrual <- .meta_accrual_flag(slots$trunc_adjusted, slots$trunc_design)
  if (slots$cens_adjusted %in% c(3, 4)) {
    # Moving the estimand along the delay axis moves where its nodes start.
    shifted <- slots
    shifted$cens_adjusted <- .meta_cens_base(slots$cens_adjusted)
    nodes <- .meta_implied_nodes(dist, args, shifted)
    nodes$origin <- nodes$origin +
      .meta_cens_shift(slots$cens_adjusted, slots$pwindow, slots$swindow)
    return(nodes)
  }
  if (slots$cens_adjusted == 0) {
    first <- .meta_grid_first(slots$lower, slots$swindow)
    mass <- .meta_grid_pmf(
      dist, args, slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
      slots$growth_rate, accrual
    )
    return(list(
      values = c(0, cumsum(mass)),
      origin = (first - 0.5) * slots$swindow,
      spacing = slots$swindow
    ))
  }
  n_quad <- .meta_n_quad()
  span <- slots$cutoff - slots$lower
  quad <- seq(slots$lower, slots$cutoff, length.out = n_quad + 1)
  if (slots$cens_adjusted == 2) {
    node_cdf <- .meta_pcens_cdf(
      quad, dist, args, slots$pwindow, slots$growth_rate
    )
  } else {
    node_cdf <- do.call(.pdist(dist), c(list(q = quad), args))
  }
  if (accrual == 1L) {
    values <- .meta_accrual_reweight(
      node_cdf, slots$lower, slots$cutoff, slots$growth_rate,
      ifelse(slots$cens_adjusted == 2, slots$pwindow / 2, 0)
    )
  } else {
    base <- node_cdf[1]
    top <- ifelse(slots$trunc_adjusted == 1, 1, node_cdf[n_quad + 1])
    if (!is.finite(top - base) || top - base <= 0) {
      values <- rep(NA_real_, n_quad + 1)
    } else {
      values <- (node_cdf - base) / (top - base)
    }
  }
  return(list(
    values = values, origin = slots$lower, spacing = span / n_quad
  ))
}

#' The delay at which an interpolated distribution function reaches `p`
#'
#' Inverts the distribution function of [.meta_implied_nodes()] by linear
#' interpolation between the two points that bracket `p`. The inverse of the
#' implied estimand has no closed form on the discrete grid, and the
#' interpolation keeps it a differentiable function of the delay distribution
#' parameters, which a root search would not.
#'
#' @param nodes The output of [.meta_implied_nodes()].
#'
#' @param p A probability.
#'
#' @returns A delay.
#'
#' @keywords internal
.meta_node_quantile <- function(nodes, p) {
  values <- nodes$values
  n <- length(values)
  if (anyNA(values)) {
    return(NA_real_)
  }
  if (p <= values[1]) {
    return(nodes$origin)
  }
  if (p >= values[n]) {
    return(nodes$origin + (n - 1) * nodes$spacing)
  }
  # Matches the forward scan of meta_family_node_quantile() in Stan.
  index <- min(which(values >= p)[1] - 1L, n - 1L)
  span <- values[index + 1] - values[index]
  frac <- ifelse(span > 0, (p - values[index]) / span, 0)
  return(nodes$origin + (index - 1 + frac) * nodes$spacing)
}

#' The summaries a study would report, one per multivariate normal member
#'
#' Member types are 1 for a mean, 2 for a standard deviation and 3 for a
#' quantile at the matching probability. Quantile members are read off the
#' implied distribution function by [.meta_node_quantile()], so they are on
#' the delay scale, matching the reported values and the covariance matrix
#' supplied with them.
#'
#' @inheritParams .meta_implied_nodes
#'
#' @param moments A summary vector from [.meta_implied_moments()] for this row
#'  and draw, or `NULL` to compute it here.
#'
#' @returns A numeric vector of implied summaries.
#'
#' @keywords internal
.meta_implied_summary_vector <- function(dist, args, slots, moments = NULL) {
  types <- slots$group_type
  implied <- rep(NA_real_, length(types))
  if (any(types != 3L)) {
    if (is.null(moments)) {
      moments <- .meta_row_moments(slots, dist, args)
    }
    implied[types == 1L] <- moments[["mean"]]
    implied[types == 2L] <- moments[["sd"]]
  }
  if (any(types == 3L)) {
    nodes <- .meta_implied_nodes(dist, args, slots)
    implied[types == 3L] <- vapply(
      slots$group_p[types == 3L],
      function(p) {
        return(.meta_node_quantile(nodes, p))
      },
      numeric(1)
    )
  }
  return(implied)
}

#' The log density of a study's summaries under a supplied covariance
#'
#' A study that cannot share its delays can report a vector of summaries with
#' a covariance matrix over them, which keeps the correlation between the
#' quantities it reports. The Cholesky factor of that matrix is built once
#' when the model data are prepared and passed to Stan, so the sampler never
#' decomposes it.
#'
#' @param y A numeric vector of reported summaries.
#'
#' @param implied A numeric vector of implied summaries from
#'  [.meta_implied_summary_vector()].
#'
#' @param chol The lower triangular Cholesky factor of the reported
#'  covariance matrix.
#'
#' @returns A log density.
#'
#' @keywords internal
.meta_multi_normal_ll <- function(y, implied, chol) {
  if (!all(is.finite(implied))) {
    return(-Inf)
  }
  residual <- forwardsolve(chol, y - implied)
  return(
    -0.5 * length(y) * log(2 * pi) -
      sum(log(diag(chol))) -
      0.5 * sum(residual^2)
  )
}

#' The joint log likelihood of a set of quantiles from one study
#'
#' Quantiles reported at probabilities \eqn{p_1 < \dots < p_k} with values
#' \eqn{y_1 < \dots < y_k} split the delay axis into the cells
#' \eqn{(0, y_1], \dots, (y_{k-1}, y_k], (y_k, \infty)}, and the number of
#' delays falling in each cell is multinomial with probabilities given by the
#' increments of the implied distribution function. This is the joint version
#' of the empirical distribution function likelihood used for a single
#' quantile, and it reduces to the exact binomial when only one quantile is
#' reported. Fitting each quantile separately ignores the positive correlation
#' between the empirical distribution function at different points, which
#' over weights a study reporting a median with an interquartile range.
#'
#' A cell whose implied probability underflows to zero while the study saw
#' delays in it gives a log likelihood of `-Inf`, which rejects the draw.
#'
#' @param y A vector of reported quantile values in increasing order.
#'
#' @param cum_count A vector of cumulative counts from
#'  [.meta_quantile_counts()].
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns A log probability mass.
#'
#' @keywords internal
.meta_quantile_set_ll <- function(y, cum_count, study_n, dist, args, slots) {
  prob <- .meta_implied_probs(y, dist, args, slots)
  if (!all(is.finite(prob))) {
    return(-Inf)
  }
  cell <- diff(c(0, prob, 1))
  count <- diff(c(0, cum_count, study_n))
  if (any(count > 0 & cell <= 0)) {
    return(-Inf)
  }
  seen <- count > 0
  return(
    lgamma(study_n + 1) -
      sum(lgamma(count + 1)) +
      sum(count[seen] * log(cell[seen]))
  )
}

#' The implied summary and its standard error for one summary row and one draw
#'
#' A standard error reported for a quantile row is on the scale of the reported
#' delay, as studies report it, so it is converted to the cumulative
#' probability scale by the delta method using [.meta_implied_density()].
#'
#' A group row stands for several summaries reported by one study, and this
#' returns the marginal of its first member, which is the reported mean of a
#' mean and standard deviation pair and the cumulative probability at the
#' smallest reported quantile of a quantile set. That marginal is what the
#' posterior predictive draws for the row. The joint log likelihood of the
#' whole group is [.meta_row_log_lik()].
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters for a single draw.
#'
#' @param moments A summary vector from [.meta_implied_moments()] for this row
#'  and draw, or `NULL` to compute it here.
#'
#' @returns A named numeric vector with elements `observed`, `implied` and
#'  `se`.
#'
#' @keywords internal
.meta_summary_terms <- function(slots, dist, args, moments = NULL) {
  if (slots$obs_type == 7L) {
    implied <- .meta_implied_summary_vector(dist, args, slots, moments)
    return(c(
      observed = slots$group_value[1],
      implied = unname(implied[1]),
      se = slots$group_chol[1, 1]
    ))
  }
  if (slots$obs_type %in% c(4L, 6L)) {
    implied <- .meta_implied_prob(
      slots$value, dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$swindow, slots$trunc_adjusted, slots$cens_adjusted,
      slots$growth_rate, slots$trunc_design
    )
    observed <- slots$quantile_p
    if (slots$report_se > 0) {
      implied_density <- .meta_implied_density(
        slots$value, dist, args, slots$lower, slots$cutoff, slots$pwindow,
        slots$swindow, slots$trunc_adjusted, slots$cens_adjusted,
        slots$growth_rate, slots$trunc_design
      )
      se <- max(implied_density * slots$report_se, .meta_min_prob_se())
    } else {
      se <- sqrt(slots$quantile_p * (1 - slots$quantile_p) / slots$study_n)
    }
  } else {
    if (is.null(moments)) {
      moments <- .meta_row_moments(slots, dist, args)
    }
    observed <- slots$value
    if (slots$obs_type == 3L) {
      implied <- moments[["sd"]]
      se <- .meta_sd_se(moments, slots$study_n)
    } else {
      implied <- moments[["mean"]]
      se <- moments[["sd"]] / sqrt(slots$study_n)
    }
    if (slots$report_se > 0) {
      se <- slots$report_se
    }
  }
  return(c(observed = unname(observed), implied = unname(implied), se = se))
}

#' The log likelihood of one meta model summary row for one draw
#'
#' Ungrouped rows use the normal approximations of [.meta_summary_terms()].
#' A group row, which stands for several summaries reported by one study, uses
#' the joint likelihood of its members: [.meta_moment_pair_ll()] for a mean and
#' a standard deviation, and [.meta_quantile_set_ll()] for a set of quantiles.
#'
#' @inheritParams .meta_summary_terms
#'
#' @returns A log density.
#'
#' @keywords internal
.meta_row_log_lik <- function(slots, dist, args, moments = NULL) {
  if (slots$obs_type == 7L) {
    return(.meta_multi_normal_ll(
      slots$group_value,
      .meta_implied_summary_vector(dist, args, slots, moments),
      slots$group_chol
    ))
  }
  if (slots$obs_type == 5L) {
    if (is.null(moments)) {
      moments <- .meta_row_moments(slots, dist, args)
    }
    return(.meta_moment_pair_ll(
      slots$group_value[1], slots$group_value[2], moments, slots$study_n
    ))
  }
  if (slots$obs_type == 6L) {
    return(.meta_quantile_set_ll(
      slots$group_value, slots$group_count, slots$study_n, dist, args, slots
    ))
  }
  summaries <- .meta_summary_terms(slots, dist, args, moments)
  return(stats::dnorm(
    summaries[["observed"]], summaries[["implied"]], summaries[["se"]],
    log = TRUE
  ))
}

#' The summaries implied by one meta model row for one draw
#'
#' @inheritParams .meta_summary_terms
#'
#' @inherit .meta_moment_vector return
#'
#' @keywords internal
.meta_row_moments <- function(slots, dist, args) {
  return(.meta_implied_moments(
    dist, args, slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
    slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate,
    slots$trunc_design
  ))
}

#' The implied summaries of one meta model row for every posterior draw
#'
#' Rows sharing a study design, the same parameter draws and the same
#' quadrature resolution imply the same summaries, so they are computed once
#' and reused. The cache is bounded and lives in the package namespace, so it
#' is never written into a fitted model object. See [.meta_draws].
#'
#' Only reported means and standard deviations need implied summaries. Quantile
#' rows work on the cumulative probability scale, so they get a list of `NULL`
#' and nothing is computed for them.
#'
#' @inheritParams .meta_summary_terms
#'
#' @param dist_args A list of named parameter lists, one per posterior draw.
#'
#' @returns A list of summary vectors, one per posterior draw.
#'
#' @keywords internal
.meta_row_draw_moments <- function(slots, dist, dist_args) {
  needs_moments <- slots$obs_type %in% c(2L, 3L, 5L) ||
    (slots$obs_type == 7L && any(slots$group_type != 3L))
  if (!needs_moments) {
    return(vector("list", length(dist_args)))
  }
  # Every field of the design is written out in full, so two different designs
  # cannot share a key. The quadrature resolution is part of the key as well,
  # because changing it changes the summaries a design implies.
  key <- paste(
    dist, length(dist_args), .meta_n_quad(), slots$trunc_adjusted,
    slots$cens_adjusted, slots$trunc_design,
    sprintf(
      "%.17g|%.17g|%.17g|%.17g|%.17g",
      slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
      slots$growth_rate
    ),
    sep = "|"
  )
  cached <- .meta_draws[[key]]
  if (!is.null(cached) && identical(cached$args, dist_args)) {
    return(cached$moments)
  }
  moments <- lapply(dist_args, function(args) {
    return(.meta_row_moments(slots, dist, args))
  })
  if (length(.meta_draws) >= .meta_draw_cache_limit()) {
    rm(list = ls(.meta_draws), envir = .meta_draws)
  }
  assign(key, list(args = dist_args, moments = moments), envir = .meta_draws)
  return(moments)
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
  dist_name <- .pcd_family_dist_name(family)
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
#' [epidist_gen_log_lik()]. Summary rows use the sampling distributions
#' described in [as_epidist_meta_model()], evaluated at the implied summaries
#' for each posterior draw. A row that stands for several summaries reported by
#' one study returns the joint log likelihood of all of them, so an observation
#' here is a group of summaries rather than a single reported value.
#'
#' @inheritParams epidist_family
#'
#' @returns A function that calculates the log likelihood for a single
#'  observation. The prep object must have the meta model `vint` and `vreal`
#'  slots.
#'
#' @seealso [brms::log_lik()] for details on the brms log likelihood interface.
#'
#' @family gen
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
    moments <- .meta_row_draw_moments(slots, dist_name, dist_args)
    lpdf <- map_dbl(seq_along(dist_args), function(draw) {
      return(.meta_row_log_lik(
        slots, dist_name, dist_args[[draw]], moments[[draw]]
      ))
    })
    lpdf <- .log_lik_weight(lpdf, i = i, prep = prep)
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
#' value. A row that stands for several summaries reported by one study
#' predicts the first of them, drawn from its marginal.
#' Predictions for summary rows are therefore not on the delay scale and
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
#' @family gen
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
    moments <- .meta_row_draw_moments(slots, dist_name, dist_args)
    draws <- map_dbl(seq_along(dist_args), function(draw) {
      summaries <- .meta_summary_terms(
        slots, dist_name, dist_args[[draw]], moments[[draw]]
      )
      return(stats::rnorm(1, summaries[["implied"]], summaries[["se"]]))
    })
    return(as.matrix(draws))
  }

  return(.predict)
}
