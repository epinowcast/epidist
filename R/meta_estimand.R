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

#' The log distribution function below which a node is severed
#'
#' Every grid and quadrature path evaluates the distribution function from
#' the study's minimum delay upwards, so for a narrow delay it reaches deep
#' into the lower tail, where the Stan primary censored distribution function
#' of `primarycensored` returns a finite value with a non finite gradient.
#' A node whose plain log distribution function is below this value holds a
#' probability below `exp(-100)`, which no moment or probability the model
#' forms can resolve, so it is treated as holding no mass before that
#' function is called. Matches the cut in `meta_family_pcens_lcdf()` and
#' `meta_family_dist_prob()` in `inst/stan/meta_model/functions.stan`.
#'
#' @returns A log probability.
#'
#' @keywords internal
.meta_log_cdf_floor <- function() {
  return(-100)
}

#' Whether a delay is certainly below the cut of the distribution function
#'
#' Decided from a closed form bound on the parameters rather than from the
#' distribution function, because in Stan the distribution function must not
#' be evaluated where it underflows: its autodiff partial is then `0 / 0`,
#' and Stan's reverse pass chains every node on the stack, so the `NaN`
#' poisons the gradient even when the value is discarded. The bounds are
#' `Phi(z) < exp(-100)` for `z < -14` for the lognormal,
#' `P(a, x) <= x^a / Gamma(a + 1)` for the gamma and `1 - exp(-y) <= y` for
#' the weibull. The generalised gamma uses the gamma bound at
#' `x = (q / scale)^shape` with `a = k`. Mirrors `meta_family_deep_tail()`
#' in Stan.
#'
#' @inheritParams .meta_dist_cdf
#'
#' @returns A logical vector.
#'
#' @keywords internal
.meta_deep_tail <- function(q, dist, args) {
  # A delay at or below zero is severed by the caller, so it only needs a
  # finite logarithm here.
  q <- pmax(q, .Machine$double.xmin)
  return(switch(dist,
    plnorm = .meta_deep_tail_lognormal(q, args),
    pgamma = .meta_deep_tail_gamma(q, args),
    pweibull = .meta_deep_tail_weibull(q, args),
    pgengamma.orig = .meta_deep_tail_gengamma(q, args),
    rep(FALSE, length(q))
  ))
}

#' Deep lower tail bounds of the delay distributions
#'
#' One per family with a closed form bound, see [.meta_deep_tail()]. They
#' mirror the Stan functions of the same name in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param q A numeric vector of positive delays.
#'
#' @param args A named list of distribution parameters.
#'
#' @returns A logical vector.
#'
#' @keywords internal
.meta_deep_tail_lognormal <- function(q, args) {
  return((log(q) - args$meanlog) / args$sdlog < -14)
}

#' @rdname dot-meta_deep_tail_lognormal
#' @keywords internal
.meta_deep_tail_gamma <- function(q, args) {
  return(
    args$shape * log(q / args$scale) - lgamma(args$shape + 1) <
      .meta_log_cdf_floor()
  )
}

#' @rdname dot-meta_deep_tail_lognormal
#' @keywords internal
.meta_deep_tail_weibull <- function(q, args) {
  return(args$shape * (log(q) - log(args$scale)) < .meta_log_cdf_floor())
}

#' @rdname dot-meta_deep_tail_lognormal
#' @keywords internal
.meta_deep_tail_gengamma <- function(q, args) {
  return(
    args$shape * args$k * log(q / args$scale) - lgamma(args$k + 1) <
      .meta_log_cdf_floor()
  )
}

#' The distribution function of the delay, severed deep in its lower tail
#'
#' @param q A numeric vector of delays.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @returns A numeric vector of cumulative probabilities, zero at or below a
#'  delay of zero, where [.meta_deep_tail()] holds, and below the cut of
#'  [.meta_log_cdf_floor()].
#'
#' @keywords internal
.meta_dist_cdf <- function(q, dist, args) {
  cdf <- do.call(.pdist(dist), c(list(q = q), args))
  severed <- q <= 0 | !is.finite(cdf) | cdf < exp(.meta_log_cdf_floor())
  cdf[severed | .meta_deep_tail(q, dist, args)] <- 0
  return(cdf)
}

#' The primary censored distribution function, guarded against underflow
#'
#' `check = FALSE` skips the validation of `pdist` and `dprimary`, which are
#' taken from `stats` and `primarycensored` and so need none, and which would
#' otherwise be repeated on every call.
#'
#' Primary distributions without an analytical solution are integrated
#' numerically, which can return a non finite or negative cumulative
#' probability deep in the lower tail. Those cases carry negligible
#' probability and are treated as zero, matching the guard in
#' `inst/stan/meta_model/functions.stan`. A delay whose plain distribution
#' function is below the cut of [.meta_log_cdf_floor()] is severed to zero
#' before the primary censored function is called, as it is in Stan, since
#' the primary censored distribution function is never above the plain one.
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
  # `check = FALSE` because `pdist` and `dprimary` are fixed functions that
  # need no validation. Validating them would evaluate each at random
  # points on every call and so advance the RNG.
  cdf <- do.call(
    primarycensored::pprimarycensored,
    c(
      list(
        q = q,
        pdist = .pdist(dist),
        pwindow = pwindow,
        dprimary = primary$dprimary,
        check = FALSE
      ),
      stats::setNames(list(primary$dprimary_args), .primary_args_name()),
      args
    )
  )
  cdf[!is.finite(cdf) | cdf < 0] <- 0
  cdf[.meta_dist_cdf(q, dist, args) <= 0] <- 0
  return(pmin(cdf, 1))
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

#' The left truncation point of the base estimand a midpoint code is built on
#'
#' `delay_min` is the smallest delay a study counted on the scale it reported,
#' so a midpoint code that moved its estimand along the delay axis dropped the
#' records whose *moved* delay fell below it. The base estimand is therefore
#' left truncated at `delay_min` moved back by the same shift. A `delay_min`
#' of zero means the study counted every delay and is left alone, because it
#' is the sentinel that selects the untruncated formulas, even for code 4 whose
#' reported delays can be negative.
#'
#' The right truncation point is not moved, because the observation time
#' bounds the underlying event, not the midpointed value.
#'
#' Matches `meta_family_cens_lower` in
#' `inst/stan/meta_model/functions.stan`.
#'
#' @param lower The study's minimum delay (its left truncation point).
#'
#' @inheritParams .meta_cens_shift
#'
#' @returns The left truncation point of the base estimand.
#'
#' @keywords internal
.meta_cens_lower <- function(lower, cens_adjusted, pwindow, swindow) {
  if (lower <= 0) {
    return(lower)
  }
  return(max(
    lower - .meta_cens_shift(cens_adjusted, pwindow, swindow), 0
  ))
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

#' The slots of the base estimand a midpoint code is built on
#'
#' Replaces the censoring adjustment code by the code it is built on and moves
#' the left truncation point with it. See [.meta_cens_base()] and
#' [.meta_cens_lower()].
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns The slots of the base estimand.
#'
#' @keywords internal
.meta_cens_slots <- function(slots) {
  shifted <- slots
  shifted$cens_adjusted <- .meta_cens_base(slots$cens_adjusted)
  shifted$lower <- .meta_cens_lower(
    slots$lower, slots$cens_adjusted, slots$pwindow, slots$swindow
  )
  return(shifted)
}

#' Whether the primary event of a meta model row is tilted
#'
#' Mirrors the primary event id of `meta_family_lpmf()` in Stan, which takes
#' the exponential growth path for every row that estimates its rate,
#' whatever value the parameter holds, and for a known rate other than zero.
#'
#' @param slots The output of [.meta_draw_slots()].
#'
#' @returns `TRUE` where the primary event is not uniform.
#'
#' @keywords internal
.meta_slots_tilted <- function(slots) {
  return(isTRUE(slots$growth_known == 0L) || slots$growth_rate != 0)
}
