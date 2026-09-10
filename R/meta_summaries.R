#' The default number of quadrature intervals for truncated continuous moments
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad_default <- function() {
  return(100L)
}

#' The smallest number of quadrature intervals used for a summary row
#'
#' Set with `options(epidist.meta_n_quad = )`, as an even number of at least
#' two. Each summary row carries its own number of intervals in its `n_quad`
#' slot, chosen by [.estimates_n_quad()] from the spread the study reported
#' so that the quadrature resolves the delay, and this is the floor of that
#' choice. Set it before building the model data, since the slot is filled
#' in then. It also lifts the cap of [.meta_n_quad_max()] when set above it.
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

#' The largest number of quadrature intervals chosen for a summary row
#'
#' Every quadrature node costs a distribution function evaluation on each
#' gradient, so the number of intervals [.estimates_n_quad()] chooses for a
#' study is capped here unless `options(epidist.meta_n_quad = )` is set
#' higher. [as_epidist_estimates_data()] warns about a study the cap leaves
#' unresolved.
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad_max <- function() {
  return(2000L)
}

#' The number of quadrature intervals a row's slots ask for
#'
#' Rows built by [as_epidist_meta_model()] carry it in their `n_quad` slot.
#' A slots list assembled by hand without one uses the floor.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_slots_n_quad <- function(slots) {
  if (is.null(slots$n_quad)) {
    return(.meta_n_quad())
  }
  return(as.integer(slots$n_quad))
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
#' the weibull. Mirrors `meta_family_deep_tail()` in Stan.
#'
#' @inheritParams .meta_dist_cdf
#'
#' @returns A logical vector.
#'
#' @keywords internal
.meta_deep_tail <- function(q, dist, args) {
  floor_log <- .meta_log_cdf_floor()
  # A delay at or below zero is severed by the caller, so it only needs a
  # finite logarithm here.
  q <- pmax(q, .Machine$double.xmin)
  if (identical(dist, "plnorm")) {
    return((log(q) - args$meanlog) / args$sdlog < -14)
  }
  if (identical(dist, "pgamma")) {
    return(
      args$shape * log(q / args$scale) - lgamma(args$shape + 1) < floor_log
    )
  }
  if (identical(dist, "pweibull")) {
    return(args$shape * (log(q) - log(args$scale)) < floor_log)
  }
  return(rep(FALSE, length(q)))
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
  cdf[.meta_dist_cdf(q, dist, args) <= 0] <- 0
  return(pmin(cdf, 1))
}

#' The smallest quantile standard error the model will use
#'
#' A quantile standard error supplied on the delay scale is used as reported,
#' held at or above this value as a guard against a standard error of zero,
#' which would give a degenerate likelihood.
#'
#' @returns A delay scale standard error.
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
#' follow up available to the cases each cell holds, before renormalising.
#' A case is seen when its primary event fell early enough for its delay to
#' complete before the calendar stop, and the primary event is known only to
#' its window. A complete primary window starting at \eqn{k w_p} holds a
#' delay of \eqn{x} from its start when \eqn{k w_p + x \le A}, so the growth
#' weighted mass of the complete windows eligible for \eqn{x} is a step
#' function of \eqn{x} that steps down at \eqn{A - j w_p}. Each cell is cut
#' at those points and every piece is weighted by that mass. When `cutoff`
#' is not a multiple of `pwindow` the last primary window is partial, of
#' length \eqn{l = A - w_p \lfloor A / w_p \rfloor}. It only holds delays up
#' to \eqn{l}, and the offset of its primary events runs over \eqn{l} rather
#' than \eqn{w_p}, so its cases follow the primary censored distribution
#' function with a window of \eqn{l}, weighted by the growth weighted length
#' of the window, and are added to the cells below \eqn{l}. This is exact
#' for any `cutoff`, `pwindow` and `swindow`, and reduces to the weight at
#' the cell's lower edge when `pwindow` and `swindow` are equal and `cutoff`
#' is a multiple of both.
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
  if (accrual != 1L) {
    cdf <- .meta_pcens_cdf(boundary, dist, args, pwindow, growth_rate)
    # Once the distribution function saturates its differences can come back
    # very slightly negative, which would leave an invalid pmf. Stan builds
    # the same cells on the log scale and drops them to zero there.
    mass <- pmax(diff(cdf), 0)
    total <- cdf[length(cdf)] - cdf[1]
    if (!is.finite(total) || total <= 0) {
      return(rep(NA_real_, length(mass)))
    }
    return(mass / total)
  }
  # The collection window holds n_full complete primary windows and, unless
  # it is a multiple of pwindow, a partial last one of length `partial`.
  # Matches meta_family_grid_pmf() in Stan.
  n_full <- floor(cutoff / pwindow + 1e-9)
  partial <- cutoff - pwindow * n_full
  has_partial <- partial > 1e-9 * pwindow
  # A complete window k is eligible for a delay of x from its start when
  # k * pwindow + x <= cutoff, so the number eligible steps down at
  # cutoff - j * pwindow. Cut the cells at those points and weight each piece
  # by the growth weighted mass of the complete windows eligible inside it.
  top <- boundary[length(boundary)]
  eligible_step <- cutoff -
    pwindow * seq_len(floor((cutoff - boundary[1]) / pwindow + 1e-9))
  edge <- sort(c(boundary, eligible_step[eligible_step <= top]))
  edge <- edge[c(TRUE, diff(edge) > 1e-9 * swindow)]
  cdf <- .meta_pcens_cdf(edge, dist, args, pwindow, growth_rate)
  piece_start <- edge[-length(edge)]
  n_eligible <- pmin(
    ceiling((cutoff - piece_start) / pwindow - 1e-9), n_full
  )
  log_weight <- .meta_log_accrual_weight(
    cutoff - pwindow * n_eligible, cutoff, growth_rate
  )
  cell <- floor(piece_start / swindow + 1e-9) - first + 1
  peak <- max(log_weight)
  extra <- 0
  if (has_partial && boundary[1] < partial) {
    # The partial window is eligible for delays up to its own length, and
    # the offset of its primary events runs over that length rather than
    # over pwindow, so its delays follow the primary censored distribution
    # function with a window of `partial`. Its mass is the growth weighted
    # length of the window.
    log_partial <- growth_rate * n_full * pwindow +
      .meta_log_accrual_weight(n_full * pwindow, cutoff, growth_rate)
    peak <- max(peak, log_partial)
    q_partial <- pmin(boundary, partial)
    q_unique <- unique(q_partial)
    cdf_partial <- .meta_pcens_cdf(
      q_unique, dist, args, partial, growth_rate
    )[match(q_partial, q_unique)]
    extra <- pmax(diff(cdf_partial), 0) * exp(log_partial - peak)
  }
  piece <- pmax(diff(cdf), 0) * exp(log_weight - peak)
  mass <- as.numeric(rowsum(piece, cell)) + extra
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
  if (identical(dist, "plnorm")) {
    var_log <- args$sdlog^2
    delay_mean <- exp(args$meanlog + var_log / 2)
    variance <- delay_mean^2 * expm1(var_log)
    kurtosis <- exp(4 * var_log) +
      2 * exp(3 * var_log) +
      3 * exp(2 * var_log) -
      3
    skewness <- (exp(var_log) + 2) * sqrt(expm1(var_log))
    moments <- .meta_moment_vector(
      delay_mean, variance, skewness * variance^1.5, kurtosis * variance^2
    )
  } else if (identical(dist, "pgamma")) {
    variance <- args$shape * args$scale^2
    moments <- .meta_moment_vector(
      args$shape * args$scale,
      variance,
      2 / sqrt(args$shape) * variance^1.5,
      (3 + 6 / args$shape) * variance^2
    )
  } else if (identical(dist, "pweibull")) {
    g <- gamma(1 + seq_len(4) / args$shape)
    variance <- args$scale^2 * (g[2] - g[1]^2)
    third <- args$scale^3 * (g[3] - 3 * g[1] * g[2] + 2 * g[1]^3)
    fourth <- args$scale^4 *
      (g[4] - 4 * g[1] * g[3] + 6 * g[1]^2 * g[2] - 3 * g[1]^4)
    moments <- .meta_moment_vector(args$scale * g[1], variance, third, fourth)
  } else {
    return(cli::cli_abort(
      "Summary estimates are not supported for the {.val {dist}} distribution."
    ))
  }
  if (!all(is.finite(moments))) {
    return(.meta_moment_failure())
  }
  return(moments)
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
#' @returns A distribution function at `n_quad + 1` equally spaced nodes.
#'
#' @keywords internal
.meta_accrual_nodes <- function(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate,
  n_quad = .meta_n_quad()
) {
  quad <- seq(lower, cutoff, length.out = n_quad + 1)
  if (cens_adjusted == 2) {
    cdf <- .meta_pcens_cdf(quad, dist, args, pwindow, growth_rate)
    weight_offset <- pwindow / 2
  } else {
    cdf <- .meta_dist_cdf(quad, dist, args)
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
  growth_rate,
  n_quad = .meta_n_quad()
) {
  if (y >= cutoff) {
    return(1)
  }
  if (y <= lower) {
    return(0)
  }
  weighted <- .meta_accrual_nodes(
    dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate, n_quad
  )
  position <- (y - lower) / (cutoff - lower) * n_quad
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
  growth_rate,
  n_quad = .meta_n_quad()
) {
  if (y >= cutoff || y <= lower) {
    return(0)
  }
  span <- cutoff - lower
  weighted <- .meta_accrual_nodes(
    dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate, n_quad
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
  trunc_design = 0L,
  n_quad = .meta_n_quad()
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    # Midpoint imputation moved every delay along the axis, so the base
    # estimand is evaluated at the reported delay moved back, and its left
    # truncation point moves with it. The cutoff does not, see
    # .meta_cens_lower().
    return(.meta_implied_prob(
      y - .meta_cens_shift(cens_adjusted, pwindow, swindow), dist, args,
      .meta_cens_lower(lower, cens_adjusted, pwindow, swindow), cutoff,
      pwindow, swindow, trunc_adjusted, .meta_cens_base(cens_adjusted),
      growth_rate, trunc_design, n_quad
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
      y, dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate,
      n_quad
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
  trunc_design = 0L,
  n_quad = .meta_n_quad()
) {
  half_width <- max(1e-6, 1e-4 * y)
  step_lwr <- max(y - half_width, lower)
  step_upr <- y + half_width
  prob_upper <- .meta_implied_prob(
    step_upr, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design, n_quad
  )
  prob_lower <- .meta_implied_prob(
    step_lwr, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
    cens_adjusted, growth_rate, trunc_design, n_quad
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
  trunc_design = 0L,
  n_quad = .meta_n_quad()
) {
  accrual <- .meta_accrual_flag(trunc_adjusted, trunc_design)
  if (cens_adjusted == 3 || cens_adjusted == 4) {
    return(.meta_implied_density(
      y - .meta_cens_shift(cens_adjusted, pwindow, swindow), dist, args,
      .meta_cens_lower(lower, cens_adjusted, pwindow, swindow), cutoff,
      pwindow, swindow, trunc_adjusted, .meta_cens_base(cens_adjusted),
      growth_rate, trunc_design, n_quad
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
      y, dist, args, lower, cutoff, pwindow, cens_adjusted, growth_rate,
      n_quad
    ))
  }
  if (cens_adjusted == 2 && growth_rate != 0) {
    return(.meta_central_difference(
      y, dist, args, lower, cutoff, pwindow, swindow, trunc_adjusted,
      cens_adjusted, growth_rate, trunc_design, n_quad
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
#' A row whose `growth_known` slot is 0 estimates its growth rate as the
#' `pgrowth` distributional parameter, so its `growth_rate` element holds one
#' value per posterior draw rather than the number in the slot. Everything
#' that works one draw at a time takes the slots of that draw from
#' [.meta_draw_slots()]. A fit made before the slot existed has no
#' `growth_known` and every row of it is known.
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
  growth_known <- prep$data$vint10[i]
  if (is.null(growth_known)) {
    growth_known <- 1L
  }
  growth_rate <- prep$data$vreal8[i]
  if (growth_known == 0L) {
    growth_rate <- as.numeric(brms::get_dpar(prep, "pgrowth", i = i))
  }
  return(list(
    obs_type = prep$data$vint1[i],
    study_n = prep$data$vint2[i],
    trunc_adjusted = prep$data$vint3[i],
    cens_adjusted = prep$data$vint4[i],
    trunc_design = prep$data$vint5[i],
    n_quad = prep$data$vint9[i],
    cutoff = prep$data$vreal1[i],
    pwindow = prep$data$vreal2[i],
    swindow = prep$data$vreal3[i],
    value = prep$data$vreal4[i],
    lower = prep$data$vreal5[i],
    report_se = prep$data$vreal6[i],
    quantile_p = prep$data$vreal7[i],
    growth_rate = growth_rate,
    growth_known = growth_known,
    group_value = as.numeric(prep$data$meta_group_value)[member],
    group_count = as.numeric(prep$data$meta_group_count)[member],
    group_lower = as.numeric(prep$data$meta_group_lower)[member],
    group_type = as.integer(prep$data$meta_group_type)[member],
    group_p = as.numeric(prep$data$meta_group_p)[member],
    group_chol = matrix(
      as.numeric(prep$data$meta_group_chol)[entry], group_len, group_len
    )
  ))
}

#' The slots of one meta model row for one posterior draw
#'
#' A row with an estimated growth rate holds one rate per draw, see
#' [.meta_row_slots()], and the implied summaries of a draw are computed
#' from the rate of that draw. A row with a known rate is returned as it is.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @param draw The posterior draw index.
#'
#' @returns The slots with a single `growth_rate`.
#'
#' @keywords internal
.meta_draw_slots <- function(slots, draw) {
  rates <- slots$growth_rate
  if (length(rates) > 1) {
    slots$growth_rate <- rates[[min(draw, length(rates))]]
  }
  return(slots)
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

#' The order statistics a set of type 1 quantiles stand for
#'
#' A type 1 quantile at probability \eqn{p} of \eqn{n} delays is the
#' \eqn{\lceil n p \rceil}th smallest of them. A quantile of integer day
#' delays reported as day \eqn{y} therefore says the study saw fewer than
#' that many delays below \eqn{y} and at least that many at or below it,
#' which are the box constraints [.meta_grid_box_ll()] fits. The rounding
#' guard matches [.meta_grid_crossing_ll()], so that \eqn{n p} landing on an
#' integer up to floating point error is not pushed up a count.
#'
#' @inheritParams .meta_quantile_counts
#'
#' @returns An integer vector of counts.
#'
#' @keywords internal
.meta_crossing_counts <- function(p, study_n) {
  return(as.integer(ceiling(study_n * p - 1e-9)))
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
    shifted <- .meta_cens_slots(slots)
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
  n_quad <- .meta_slots_n_quad(slots)
  if (accrual == 1L) {
    nodes <- .meta_accrual_nodes(
      dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$cens_adjusted, slots$growth_rate, n_quad
    )
    return(.meta_interpolate(
      y - slots$lower, nodes, (slots$cutoff - slots$lower) / n_quad, 0
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
    trunc_design = slots$trunc_design, n_quad = n_quad
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
    nodes <- .meta_implied_nodes(dist, args, .meta_cens_slots(slots))
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
  n_quad <- .meta_slots_n_quad(slots)
  span <- slots$cutoff - slots$lower
  quad <- seq(slots$lower, slots$cutoff, length.out = n_quad + 1)
  if (slots$cens_adjusted == 2) {
    node_cdf <- .meta_pcens_cdf(
      quad, dist, args, slots$pwindow, slots$growth_rate
    )
  } else {
    node_cdf <- .meta_dist_cdf(quad, dist, args)
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

#' The delay at which an implied distribution function reaches `p`
#'
#' Inverts the distribution function of [.meta_implied_nodes()] by linear
#' interpolation between the two points that bracket `p`. On the discrete
#' grid that interpolant is the model's own definition of the continuity
#' corrected quantile, so the chord is exact there. For a continuous estimand
#' the chord is only as accurate as the node spacing, which for a truncation
#' adjusted study is about a day, so it is refined by
#' [.meta_refine_quantile()] when the design is supplied. The result stays a
#' differentiable function of the delay distribution parameters, which a root
#' search would not be.
#'
#' @param nodes The output of [.meta_implied_nodes()].
#'
#' @param p A probability.
#'
#' @param dist A `primarycensored` distribution function name, or `NULL` to
#'  return the chord inverse alone.
#'
#' @param args A named list of distribution parameters.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns A delay.
#'
#' @keywords internal
.meta_node_quantile <- function(
  nodes,
  p,
  dist = NULL,
  args = NULL,
  slots = NULL
) {
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
  chord <- nodes$origin + (index - 1 + frac) * nodes$spacing
  if (is.null(slots)) {
    return(chord)
  }
  return(.meta_refine_quantile(
    chord, p, dist, args, slots,
    floor = nodes$origin, ceiling = nodes$origin + (n - 1) * nodes$spacing
  ))
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

#' The number of Newton steps taken from the chord inverse
#'
#' Matches `meta_family_node_quantile()` in Stan. Each step squares the
#' error of the chord, which is of the order of the node spacing squared, so
#' two steps leave a residual well below the sampling standard error of any
#' reported quantile.
#'
#' @returns An integer.
#'
#' @keywords internal
.meta_newton_steps <- function() {
  return(2L)
}

#' Refine the chord inverse of a continuous implied distribution function
#'
#' Where the family quantile function is available, that is for a lognormal
#' or weibull delay reported by a study that adjusted for censoring and did
#' not use an accrual design, the implied quantile is
#' \eqn{Q(F(L) + p (F(D) - F(L)))} exactly. Otherwise Newton steps are taken
#' from the chord using the implied distribution function and density of
#' [.meta_implied_prob()] and [.meta_implied_density()], which exist in
#' closed form for every remaining continuous design with a uniform primary
#' event. An accrual estimand, or a uniform single interval estimand with a
#' growing primary event, is defined by linear interpolation between its
#' nodes, so its chord is left alone, as is a discrete grid.
#'
#' The Stan mirror `meta_family_node_quantile()` cannot evaluate the primary
#' censored distribution function at a parameter dependent delay, so the
#' cases refined here are exactly those it can refine with closed forms.
#'
#' @param chord The chord inverse from [.meta_node_quantile()].
#'
#' @inheritParams .meta_node_quantile
#'
#' @param floor,ceiling The delays at the first and last node, which the
#'  refined value is held between.
#'
#' @returns A delay.
#'
#' @keywords internal
.meta_refine_quantile <- function(
  chord,
  p,
  dist,
  args,
  slots,
  floor,
  ceiling
) {
  if (.meta_quantile_on_chord(slots)) {
    return(chord)
  }
  base_code <- .meta_cens_base(slots$cens_adjusted)
  clamp <- function(value) {
    return(min(max(value, floor), ceiling))
  }
  if (base_code == 1L && dist %in% c("plnorm", "pweibull")) {
    pdist <- .pdist(dist)
    base <- 0
    if (slots$lower > 0) {
      base <- do.call(pdist, c(list(q = slots$lower), args))
    }
    top <- 1
    if (slots$trunc_adjusted != 1) {
      top <- do.call(pdist, c(list(q = slots$cutoff), args))
    }
    exact <- do.call(
      .estimates_qdist(dist), c(list(p = base + p * (top - base)), args)
    )
    if (!is.finite(exact)) {
      return(chord)
    }
    return(clamp(exact))
  }
  value <- chord
  n_quad <- .meta_slots_n_quad(slots)
  for (step in seq_len(.meta_newton_steps())) {
    prob <- .meta_implied_prob(
      value, dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$swindow, slots$trunc_adjusted, slots$cens_adjusted,
      slots$growth_rate, slots$trunc_design, n_quad
    )
    slope <- .meta_implied_density(
      value, dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$swindow, slots$trunc_adjusted, slots$cens_adjusted,
      slots$growth_rate, slots$trunc_design, n_quad
    )
    if (!is.finite(prob) || !is.finite(slope) || slope <= 0) {
      return(value)
    }
    value <- clamp(value + (p - prob) / slope)
  }
  return(value)
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
        return(.meta_node_quantile(nodes, p, dist, args, slots))
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

#' Whether the implied quantile of a design is left on its chord
#'
#' The chord inverse of [.meta_node_quantile()] is refined only where the
#' implied distribution function and density exist in closed form. A discrete
#' grid, an accrual estimand and a uniform single interval estimand with a
#' growing primary event are defined by the interpolation between their
#' nodes, so their chord is the implied quantile and the slope of the interval
#' holding it is the implied density. See [.meta_refine_quantile()]. Matches
#' the early returns of `meta_family_node_quantile()` in Stan.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns A logical scalar.
#'
#' @keywords internal
.meta_quantile_on_chord <- function(slots) {
  base_code <- .meta_cens_base(slots$cens_adjusted)
  accrual <- .meta_accrual_flag(slots$trunc_adjusted, slots$trunc_design)
  return(
    base_code == 0L || accrual == 1L ||
      (base_code == 2L && .meta_slots_tilted(slots))
  )
}

#' The index of the node interval holding a delay
#'
#' Interval `i` runs from node `i` to node `i + 1`. It is found by stepping
#' through the nodes rather than by rounding, because in Stan the delay is a
#' parameter and cannot be converted to an integer. Matches
#' `meta_family_node_interval()` in Stan.
#'
#' @inheritParams .meta_node_quantile
#'
#' @param q A delay between the first and last node.
#'
#' @returns An integer between 1 and the number of intervals.
#'
#' @keywords internal
.meta_node_interval <- function(nodes, q) {
  n_interval <- length(nodes$values) - 1L
  index <- 1L
  while (index < n_interval && nodes$origin + index * nodes$spacing <= q) {
    index <- index + 1L
  }
  return(index)
}

#' The density of a continuous estimand at its implied quantile
#'
#' Where the quantile is left on its chord, see [.meta_quantile_on_chord()],
#' the estimand is the linear interpolant of its nodes and its density is the
#' slope of the interval holding the quantile. Otherwise it is the closed
#' form density of [.meta_implied_density()]. Matches
#' `meta_family_quantile_density()` in Stan.
#'
#' @param q The implied quantile from [.meta_node_quantile()].
#'
#' @param index The node interval holding `q`, from [.meta_node_interval()].
#'
#' @inheritParams .meta_node_quantile
#'
#' @returns A density on the delay scale.
#'
#' @keywords internal
.meta_quantile_density <- function(q, index, nodes, dist, args, slots) {
  if (.meta_quantile_on_chord(slots)) {
    return((nodes$values[index + 1] - nodes$values[index]) / nodes$spacing)
  }
  return(.meta_implied_density(
    q, dist, args, slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
    slots$trunc_adjusted, slots$cens_adjusted, slots$growth_rate,
    slots$trunc_design, .meta_slots_n_quad(slots)
  ))
}

#' The centred partial moments of an estimand below its implied quantile
#'
#' The sampling covariance of a reported mean or standard deviation with a
#' reported quantile depends on \eqn{\int_L^{q} (x - \mu)^k \text{d}G(x)} for
#' \eqn{k = 1, 2}, with \eqn{G} the implied distribution function, \eqn{\mu}
#' the implied mean and \eqn{L} the smallest delay the study counted.
#' Integrating by parts gives
#' \eqn{(q - \mu)^k p - \int_L^{q} k (x - \mu)^{k - 1} G(x) \text{d}x}, and
#' the remaining integral is taken by the trapezoid rule over the linear
#' interpolant of the nodes, which reaches `p` at `q`. Its error is of the
#' order of the node spacing squared, which [.estimates_n_quad()] holds to a
#' quarter of the reported spread. Matches `meta_family_quantile_partials()`
#' in Stan.
#'
#' @inheritParams .meta_quantile_density
#'
#' @param p The probability of the quantile.
#'
#' @param centre The implied mean of the estimand.
#'
#' @returns A numeric vector of the first and second centred partial moments.
#'
#' @keywords internal
.meta_quantile_partials <- function(nodes, q, p, index, centre) {
  kept <- seq_len(index)
  x <- nodes$origin + (kept - 1) * nodes$spacing - centre
  g <- nodes$values[kept]
  first <- x * g
  spacing <- nodes$spacing
  top <- q - centre
  remainder <- q - (nodes$origin + (index - 1) * nodes$spacing)
  int_g <- sum((g[-1] + g[-index]) / 2) * spacing +
    (g[index] + p) / 2 * remainder
  int_first <- sum((first[-1] + first[-index]) / 2) * spacing +
    (first[index] + top * p) / 2 * remainder
  return(c(top * p - int_g, top^2 * p - 2 * int_first))
}

#' The sampling covariance of the summaries one study reports
#'
#' The asymptotic covariance of a sample mean, a sample standard deviation
#' and sample quantiles computed from the same `study_n` delays. The mean and
#' standard deviation block is that of [.meta_moment_pair_ll()]. By the
#' Bahadur representation a sample quantile at probability \eqn{p} is
#' \eqn{Q_p - (\hat{G}(Q_p) - p) / f(Q_p)} up to a smaller order term, so
#' with \eqn{f_i = f(Q_{p_i})} the implied density at each implied quantile
#' \deqn{\text{Cov}(q_i, q_j) = \frac{p_i (1 - p_j)}{n f_i f_j}, \quad
#' p_i \le p_j,}
#' \deqn{\text{Cov}(\bar{x}, q_i) = -\frac{1}{n f_i}
#' \int_L^{Q_{p_i}} (x - \mu) \text{d}G(x),}
#' \deqn{\text{Cov}(s, q_i) = -\frac{1}{2 \sigma n f_i}
#' \int_L^{Q_{p_i}} \left((x - \mu)^2 - \sigma^2\right) \text{d}G(x),}
#' the last carried from the sample variance to the standard deviation by
#' the delta method. The integrals are the centred partial moments of
#' [.meta_quantile_partials()]. Matches `meta_family_joint_covariance()` in
#' Stan.
#'
#' @param types Member types, 1 for a mean, 2 for a standard deviation and 3
#'  for a quantile.
#'
#' @param probs Member probabilities, zero for a mean or standard deviation.
#'
#' @param moments A summary vector from [.meta_moment_vector()].
#'
#' @param density The implied density at each implied quantile, one per
#'  quantile member in order.
#'
#' @param partial A two row matrix of centred partial moments from
#'  [.meta_quantile_partials()], one column per quantile member in order.
#'
#' @param study_n The number of delays the summaries were computed from.
#'
#' @returns A covariance matrix over the members.
#'
#' @keywords internal
.meta_joint_covariance <- function(
  types,
  probs,
  moments,
  density,
  partial,
  study_n
) {
  k <- length(types)
  spread <- moments[["sd"]]
  se_mean <- spread / sqrt(study_n)
  se_sd <- .meta_sd_se(moments, study_n)
  rho <- .meta_moment_correlation(moments)
  position <- cumsum(types == 3L)
  entry <- function(i, j) {
    if (types[i] == 3L && types[j] == 3L) {
      return(
        min(probs[i], probs[j]) * (1 - max(probs[i], probs[j])) /
          (study_n * density[position[i]] * density[position[j]])
      )
    }
    if (types[i] == 3L) {
      return(entry(j, i))
    }
    if (types[j] == 3L) {
      m <- position[j]
      if (types[i] == 1L) {
        return(-partial[1, m] / (study_n * density[m]))
      }
      return(
        -(partial[2, m] - spread^2 * probs[j]) /
          (2 * spread * study_n * density[m])
      )
    }
    if (types[i] != types[j]) {
      return(rho * se_mean * se_sd)
    }
    if (types[i] == 1L) {
      return(se_mean^2)
    }
    return(se_sd^2)
  }
  covariance <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      covariance[i, j] <- entry(i, j)
    }
  }
  return(covariance)
}

#' The implied summaries of a joint study group and their sampling covariance
#'
#' A continuous estimand reporting a mean or a standard deviation alongside
#' quantiles has every summary fitted jointly, with the covariance of
#' [.meta_joint_covariance()] derived from the implied distribution rather
#' than supplied. The quantiles are read off the implied nodes as for a
#' covariance matrix group, see [.meta_implied_summary_vector()], and the
#' density and centred partial moments at each are taken from the same nodes.
#'
#' @inheritParams .meta_implied_summary_vector
#'
#' @returns A list with the `implied` summary vector and the covariance
#'  matrix `sigma`, or `NULL` where the implied moments are not finite, the
#'  nodes underflow or a quantile sits where the estimand has no density,
#'  which the caller rejects.
#'
#' @keywords internal
.meta_joint_study_terms <- function(dist, args, slots, moments = NULL) {
  types <- slots$group_type
  if (is.null(moments)) {
    moments <- .meta_row_moments(slots, dist, args)
  }
  if (!all(is.finite(moments))) {
    return(NULL)
  }
  implied <- rep(NA_real_, length(types))
  implied[types == 1L] <- moments[["mean"]]
  implied[types == 2L] <- moments[["sd"]]
  quantile_at <- which(types == 3L)
  densities <- numeric(length(quantile_at))
  partial <- matrix(0, 2, length(quantile_at))
  if (length(quantile_at) > 0) {
    nodes <- .meta_implied_nodes(dist, args, slots)
    if (anyNA(nodes$values)) {
      return(NULL)
    }
    for (m in seq_along(quantile_at)) {
      p <- slots$group_p[quantile_at[m]]
      implied_q <- .meta_node_quantile(nodes, p, dist, args, slots)
      index <- .meta_node_interval(nodes, implied_q)
      f <- .meta_quantile_density(implied_q, index, nodes, dist, args, slots)
      if (!is.finite(f) || f <= 0) {
        return(NULL)
      }
      implied[quantile_at[m]] <- implied_q
      densities[m] <- f
      partial[, m] <- .meta_quantile_partials(
        nodes, implied_q, p, index, moments[["mean"]]
      )
    }
  }
  covariance <- .meta_joint_covariance(
    types, slots$group_p, moments, densities, partial, slots$study_n
  )
  return(list(implied = implied, sigma = covariance))
}

#' The joint log likelihood of the summaries of one continuous study
#'
#' The multivariate normal of [.meta_joint_study_terms()]. A covariance that
#' is not positive definite, which quadrature error can produce for a draw
#' far from the reported summaries, is rejected with a log likelihood of
#' `-Inf`, as are the failures listed there. Matches
#' `meta_family_joint_study_lpdf()` in Stan.
#'
#' @param y A numeric vector of reported summaries in member order.
#'
#' @inheritParams .meta_joint_study_terms
#'
#' @returns A log density.
#'
#' @keywords internal
.meta_joint_study_ll <- function(y, dist, args, slots, moments = NULL) {
  pieces <- .meta_joint_study_terms(dist, args, slots, moments)
  if (is.null(pieces)) {
    return(-Inf)
  }
  chol_lower <- tryCatch(
    t(chol(pieces$sigma)),
    error = function(e) {
      return(NULL)
    }
  )
  if (is.null(chol_lower)) {
    return(-Inf)
  }
  return(.meta_multi_normal_ll(y, pieces$implied, chol_lower))
}

#' The smallest probability a multinomial cell is given
#'
#' A cell whose implied probability underflows to zero while the study saw
#' delays in it would give a log likelihood of `-Inf`. Stan differences log
#' distribution functions and so keeps a tiny mass in such a cell, where R
#' differences them on the natural scale and gets exactly zero. Both floor
#' the cell here, so that a single badly misfitting draw is rejected in
#' practice but leaves `loo()` a finite value.
#'
#' @returns A probability.
#'
#' @keywords internal
.meta_cell_floor <- function() {
  return(1e-300)
}

#' The log of a difference of two exponentials
#'
#' Matches Stan's `log_diff_exp()`, returning `-Inf` where the difference is
#' zero or negative rather than `NaN`.
#'
#' @param upper,lower Logarithms, with `upper` expected to be the larger.
#'
#' @returns `log(exp(upper) - exp(lower))`.
#'
#' @keywords internal
.meta_log_diff_exp <- function(upper, lower) {
  if (lower == -Inf) {
    return(upper)
  }
  if (upper == -Inf || upper <= lower) {
    return(-Inf)
  }
  return(upper + log(-expm1(lower - upper)))
}

#' The log likelihood of a single quantile of integer day delays
#'
#' A quantile of delays counted in whole censoring windows is a discrete
#' statistic. "The median is 5 days" says that the empirical distribution
#' function crossed one half between 4 and 5 days, that is
#' \eqn{N_{\le y - w_s} < \lceil n p \rceil \le N_{\le y}} with
#' \eqn{N_{\le y}} the number of delays at or below \eqn{y}, which reads the
#' reported value as a type 1 quantile. Each count is binomial on the
#' uncorrected grid distribution function, so the probability of the event
#' is a difference of two binomial upper tails, computed on the log scale.
#'
#' Unlike the continuity corrected forms, the information this carries
#' saturates as the study grows: once the binomial spread of the crossing is
#' narrower than a window the reported integer stops moving, and the
#' likelihood tends to an indicator of the parameters that put the
#' population quantile in the reported cell.
#'
#' Matches `meta_family_grid_crossing_ll()` in Stan.
#'
#' @param y The reported quantile value.
#'
#' @param p The probability the quantile was reported at.
#'
#' @inheritParams .meta_quantile_set_ll
#'
#' @returns A log probability mass.
#'
#' @keywords internal
.meta_grid_crossing_ll <- function(y, p, study_n, dist, args, slots) {
  y <- y - .meta_cens_shift(slots$cens_adjusted, slots$pwindow, slots$swindow)
  accrual <- .meta_accrual_flag(slots$trunc_adjusted, slots$trunc_design)
  n_grid <- floor(slots$cutoff / slots$swindow)
  first <- .meta_grid_first(slots$lower, slots$swindow)
  cell <- floor(y / slots$swindow + 0.5)
  if (cell < first || cell >= n_grid) {
    return(-Inf)
  }
  if (accrual != 1L) {
    edges <- .meta_grid_edges(
      cell, dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$swindow, slots$growth_rate
    )
  } else {
    mass <- .meta_grid_pmf(
      dist, args, slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
      slots$growth_rate, accrual
    )
    if (anyNA(mass)) {
      return(-Inf)
    }
    edges <- c(0, cumsum(mass))[cell - first + 1:2]
  }
  if (!all(is.finite(edges))) {
    return(-Inf)
  }
  edges <- pmin(pmax(edges, 0), 1)
  # Matches the rounding guard of the Stan mirror, so that n p landing on an
  # integer up to floating point error is not pushed up a count.
  k <- ceiling(study_n * p - 1e-9)
  if (k < 1) {
    return(-Inf)
  }
  # The crossing N_{<= y - w} < k <= N_{<= y} is the difference of two
  # binomial tails, taken on the side where both are small so that it does
  # not cancel: the upper tails when the count the estimand expects at the
  # cell is below k, the lower tails otherwise. Every tail goes through
  # .meta_log_binom_upper(), which stays finite far out. The difference
  # itself underflows only when the estimand puts the reported quantile far
  # into its tail, as it does at a random initial value, and the sum over
  # the count below the cell then covers it.
  if (k <= study_n * edges[1]) {
    tails <- .meta_log_binom_upper(
      study_n - k + 1, study_n, 1 - edges
    )
    log_hi <- tails[1]
    log_lo <- tails[2]
  } else {
    tails <- .meta_log_binom_upper(k, study_n, edges)
    log_hi <- tails[2]
    log_lo <- tails[1]
  }
  if (is.finite(log_hi) && log_hi - log_lo > 1e-8) {
    return(.meta_log_diff_exp(log_hi, log_lo))
  }
  log_above <- log(max(1 - edges[1], 1e-300))
  log_cell <- log(max(edges[1], 1e-300))
  cell_mass <- min(
    max((edges[2] - edges[1]) / max(1 - edges[1], 1e-300), 1e-300), 1
  )
  j <- seq_len(k) - 1
  log_count <- lchoose(study_n, j) + (study_n - j) * log_above +
    j * log_cell
  log_terms <- log_count +
    .meta_log_binom_upper(k - j, study_n - j, cell_mass)
  return(.meta_log_sum_exp(log_terms))
}

#' The log upper tail of a binomial count, stable far into the tail
#'
#' `P(M >= m)` for `M ~ Binomial(size, prob)`. Nine standard deviations
#' above the mean, or below a probability of 1e-12, the tail is summed term
#' by term on the log scale until the terms fall forty nats below the first,
#' or two hundred terms in with a geometric bound on the rest, which is exact
#' to that tolerance and, unlike the distribution function, has finite
#' partial derivatives there in Stan. Elsewhere it is the
#' distribution function of the complement. Matches
#' `meta_family_log_binom_upper()` in Stan.
#'
#' @param m The smallest count in the tail, a vector.
#'
#' @param size The number of trials, a vector.
#'
#' @param prob The success probability, a vector.
#'
#' @returns The log tail probabilities, recycled to the longest argument.
#'
#' @keywords internal
.meta_log_binom_upper <- function(m, size, prob) {
  len <- max(length(m), length(size), length(prob))
  m <- rep_len(m, len)
  size <- rep_len(size, len)
  prob <- rep_len(prob, len)
  out <- numeric(len)
  out[m > size & prob < 1] <- -Inf
  active <- m > 0 & m <= size & prob < 1
  m <- m[active]
  size <- size[active]
  prob <- prob[active]
  excess <- m - 1 - size * prob
  far <- prob < 1e-12 |
    (excess > 0 & excess^2 > 81 * size * prob * (1 - prob))
  res <- numeric(length(m))
  res[far] <- vapply(
    which(far),
    function(i) {
      last <- min(size[i], m[i] + 200)
      counts <- m[i]:last
      log_terms <- lchoose(size[i], counts) +
        counts * log(max(prob[i], 1e-300)) +
        (size[i] - counts) * log1p(-prob[i])
      keep <- log_terms >= log_terms[1] - 40
      if (all(keep) && last < size[i]) {
        # The remaining terms fall at least as fast as the last ratio, so
        # their sum is bounded by a geometric series in it.
        ratio <- (size[i] - last) * prob[i] / ((last + 1) * (1 - prob[i]))
        log_terms <- c(
          log_terms,
          log_terms[length(log_terms)] + log(ratio) -
            log1p(-min(ratio, 0.999))
        )
        keep <- c(keep, TRUE)
      }
      return(.meta_log_sum_exp(log_terms[keep]))
    },
    numeric(1)
  )
  res[!far] <- stats::pbinom(
    m[!far] - 1, size[!far], prob[!far],
    lower.tail = FALSE, log.p = TRUE
  )
  out[active] <- res
  return(out)
}

#' A numerically stable log of a sum of exponentials
#'
#' @param x A numeric vector of log values.
#'
#' @returns `log(sum(exp(x)))`, or `-Inf` when every element is `-Inf`.
#'
#' @keywords internal
.meta_log_sum_exp <- function(x) {
  top <- max(x)
  if (!is.finite(top)) {
    return(top)
  }
  return(top + log(sum(exp(x - top))))
}

#' The uncorrected grid distribution function at a set of cells
#'
#' The number of delays a study saw at or below a grid cell is binomial on
#' the grid distribution function at that cell, which is the mass of the
#' cells up to and including it. A cohort grid is normalised by the mass it
#' holds, so only the cells asked for, the top of the grid and its first cell
#' are evaluated, as in [.meta_grid_edges()]. An accrual grid reweights every
#' cell before renormalising, so it is built in full by [.meta_grid_pmf()].
#'
#' @param cell An integer vector of grid cell indices, counting from zero.
#'
#' @param accrual 1 to apply the accrual weight, 0 otherwise.
#'
#' @inheritParams .meta_grid_pmf
#'
#' @returns The grid distribution function at each cell, or infinities if
#'  the grid mass underflows to zero.
#'
#' @keywords internal
.meta_grid_cdf <- function(
  cell,
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
  if (accrual == 1L) {
    mass <- .meta_grid_pmf(
      dist, args, lower, cutoff, pwindow, swindow, growth_rate, accrual
    )
    if (anyNA(mass)) {
      return(rep(Inf, length(cell)))
    }
    return(c(0, cumsum(mass))[cell - first + 2])
  }
  top <- pmin(cell + 1, n_grid)
  point <- unique(c(top, n_grid, first))
  cdf <- .meta_pcens_cdf(point * swindow, dist, args, pwindow, growth_rate)
  base <- cdf[match(first, point)]
  total <- cdf[match(n_grid, point)] - base
  if (!is.finite(total) || total <= 0) {
    return(rep(Inf, length(cell)))
  }
  return((cdf[match(top, point)] - base) / total)
}

#' The stride of the coarse grid of counts the forward pass centres on
#'
#' The most likely path of the constrained chain of counts is found on a
#' grid of every `stride`th count, about half a standard deviation of a
#' binomial apart, so that a search over it costs about four times the
#' sample size per edge. Matches `meta_family_box_stride()` in Stan.
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @returns An integer stride of at least one.
#'
#' @keywords internal
.meta_box_stride <- function(study_n) {
  return(max(1L, as.integer(ceiling(sqrt(study_n) / 2))))
}

#' The half width of the band of counts the forward pass keeps
#'
#' The forward pass of [.meta_grid_box_ll()] sums over the counts within
#' six standard deviations and eight counts of the most likely path, plus
#' the stride of the grid that path was found on. The mass beyond six
#' standard deviations is below 1e-9 of the total for every binomial,
#' including the Poisson like tail of a step with a small mean, which the
#' eight counts cover. Matches `meta_family_band_half_width()` in Stan.
#'
#' @param variance The variance of the count.
#'
#' @param stride The stride of the coarse grid from [.meta_box_stride()].
#'
#' @returns An integer number of counts.
#'
#' @keywords internal
.meta_band_half_width <- function(variance, stride) {
  return(as.integer(ceiling(6 * sqrt(max(variance, 0))) + 8L + stride))
}

#' The counts of a coarse grid within a box
#'
#' The box boundaries and every multiple of the stride between them, so that
#' a most likely count sitting on a boundary is on the grid.
#'
#' @param lower,upper The box.
#'
#' @param stride The stride of the grid from [.meta_box_stride()].
#'
#' @returns An increasing integer vector of counts.
#'
#' @keywords internal
.meta_box_grid <- function(lower, upper, stride) {
  first <- ceiling(lower / stride)
  last <- floor(upper / stride)
  inner <- if (first <= last) stride * seq.int(first, last) else integer(0)
  return(as.integer(sort(unique(c(lower, inner, upper)))))
}

#' The log mass of a binomial count with floored probabilities
#'
#' The binomial mass with the success and failure probabilities floored at
#' 1e-300, so that a count the parameters make all but impossible is very
#' unlikely rather than impossible and a chain can start from a random
#' initial value. Matches the arithmetic of `meta_family_grid_box_ll()` in
#' Stan.
#'
#' @param x A vector of counts.
#'
#' @param size A vector of numbers of trials.
#'
#' @param prob The success probability.
#'
#' @returns A vector of log masses.
#'
#' @keywords internal
.meta_log_binom <- function(x, size, prob) {
  return(
    lchoose(size, x) + x * log(max(prob, 1e-300)) +
      (size - x) * log(max(1 - prob, 1e-300))
  )
}

#' The probability of a step of the chain of cumulative counts
#'
#' Given the count at one edge, the delays beyond it fall at or below the
#' next edge with this probability. A distribution function that has
#' reached one leaves nothing to place, so the step probability is zero.
#'
#' @param from,to The grid distribution function at the two edges.
#'
#' @returns A probability.
#'
#' @keywords internal
.meta_step_prob <- function(from, to) {
  if (1 - from <= 0) {
    return(0)
  }
  return(min(max((to - from) / (1 - from), 0), 1))
}

#' The most likely path of the constrained chain of cumulative counts
#'
#' A Viterbi pass over the coarse grid of [.meta_box_grid()] at every edge,
#' which finds the counts a study most probably had at each edge given
#' every box, to within a stride. The forward pass of [.meta_grid_box_ll()]
#' is then kept to a band around them. Centring on the counts the boxes
#' pull the chain to, rather than on the counts the parameters expect at
#' each edge, is what keeps the pass accurate where a later quantile forces
#' an earlier count far from its mean. The distribution function is rounded
#' to \eqn{2^{-20}} by [.meta_fixed_point()] first, as Stan must to keep the
#' search off the autodiff stack. Matches `meta_family_box_mode_path()` in
#' Stan.
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @param cdf The grid distribution function at the edges.
#'
#' @param lower,upper The boxes at the edges.
#'
#' @param stride The stride of the grid from [.meta_box_stride()].
#'
#' @returns An integer vector of counts, one per edge, or `NULL` when no
#'  path satisfies every box.
#'
#' @keywords internal
.meta_box_mode_path <- function(study_n, cdf, lower, upper, stride) {
  n_edge <- length(cdf)
  cdf <- .meta_fixed_point(cdf) / 2^20
  counts <- Map(.meta_box_grid, lower, upper, stride = stride)
  value <- .meta_log_binom(counts[[1]], study_n, cdf[1])
  back <- vector("list", n_edge)
  for (i in seq_len(n_edge)[-1]) {
    r <- .meta_step_prob(cdf[i - 1], cdf[i])
    from <- counts[[i - 1]]
    target <- counts[[i]]
    d <- outer(target, from, "-")
    reachable <- d >= 0
    log_terms <- matrix(-Inf, length(target), length(from))
    log_terms[reachable] <- rep(value, each = length(target))[reachable] +
      .meta_log_binom(
        d[reachable], rep(study_n - from, each = length(target))[reachable],
        r
      )
    back[[i]] <- max.col(log_terms, ties.method = "first")
    value <- log_terms[cbind(seq_along(target), back[[i]])]
  }
  if (!is.finite(max(value))) {
    return(NULL)
  }
  path <- integer(n_edge)
  j <- which.max(value)
  for (i in rev(seq_len(n_edge))) {
    path[i] <- counts[[i]][j]
    if (i > 1) {
      j <- back[[i]][j]
    }
  }
  return(path)
}

#' The log of a sum of exponentials over each row of a matrix
#'
#' @param m A numeric matrix of log values.
#'
#' @returns A numeric vector, one value per row, `-Inf` where every element
#'  of the row is `-Inf`.
#'
#' @keywords internal
.meta_row_log_sum_exp <- function(m) {
  top <- apply(m, 1, max)
  shift <- ifelse(is.finite(top), top, 0)
  return(shift + log(rowSums(exp(m - shift))))
}

#' A number as a multiple of a power of two, for a data only scaling
#'
#' The forward pass scales each step around the expected size of the step,
#' and centres its bands on the most likely path, both of which depend on
#' the parameters. Stan can only build data from integers, so each is
#' carried as the integer part of \eqn{2^{20}} times the number, found
#' there by bisection on comparisons. Neither has to be exact: the result
#' of a step does not depend on its scaling, and the bands are wide. Matches
#' `meta_family_fixed_point()` in Stan.
#'
#' @param x A number.
#'
#' @returns An integer vector within \eqn{\pm 2^{29}}.
#'
#' @keywords internal
.meta_fixed_point <- function(x) {
  return(as.integer(pmin(pmax(floor(x * 2^20), -2^29), 2^29)))
}

#' The log kernel of a forward pass step at its expected size
#'
#' The kernel of a step is \eqn{\exp(d \lambda - \log d!)} at a step of
#' \eqn{d} counts, with \eqn{\lambda} the log expected step, and is divided
#' by its value at the expected step so that no entry exceeds one. Matches
#' `meta_family_step_kernel_top()` in Stan.
#'
#' @param lambda_scaled The log expected step from [.meta_fixed_point()].
#'
#' @returns A number.
#'
#' @keywords internal
.meta_step_kernel_top <- function(lambda_scaled) {
  lambda <- lambda_scaled / 2^20
  expected <- floor(min(exp(lambda), 1e9))
  return(expected * lambda - lgamma(expected + 1))
}

#' Whether the kernel of a forward pass step is in range at its likely size
#'
#' The kernel falls away from the expected step like a Poisson mass, so it
#' underflows at a step hundreds of nats from the expected one. The boxes
#' force such a step only where the parameters are that far from fitting
#' the study, and the step then takes the slower path that sums on the log
#' scale. Matches `meta_family_step_kernel_alive()` in Stan.
#'
#' @param d_star The step of the most likely path.
#'
#' @inheritParams .meta_step_kernel_top
#'
#' @returns `TRUE` or `FALSE`.
#'
#' @keywords internal
.meta_step_kernel_alive <- function(d_star, lambda_scaled) {
  lambda <- lambda_scaled / 2^20
  return(
    d_star * lambda - lgamma(d_star + 1) -
      .meta_step_kernel_top(lambda_scaled) > -400
  )
}

#' The kernel of a forward pass step as a matrix
#'
#' The binomial mass of a step from \eqn{s} to \eqn{t} counts splits into a
#' factor in \eqn{s}, a factor in \eqn{t} and \eqn{1 / (t - s)!}, so the
#' step is a matrix in \eqn{t - s} times a vector in \eqn{s}. The matrix
#' holds \eqn{\exp((t - s) \lambda - \log (t - s)!)} scaled by
#' [.meta_step_kernel_top()], which keeps its entries in range around the
#' expected step, and is zero where \eqn{t < s}. It depends on the parameters
#' only through the scaling, so Stan builds it as data and the step costs
#' one matrix product on the autodiff stack rather than an entry per pair
#' of counts. Matches `meta_family_step_kernel()` in Stan.
#'
#' @param a,b The first and last source count.
#'
#' @param a2,b2 The first and last target count.
#'
#' @inheritParams .meta_step_kernel_top
#'
#' @returns A matrix with a row per target and a column per source count.
#'
#' @keywords internal
.meta_step_kernel <- function(a, b, a2, b2, lambda_scaled) {
  lambda <- lambda_scaled / 2^20
  top <- .meta_step_kernel_top(lambda_scaled)
  d_min <- max(0, a2 - b)
  d_max <- b2 - a
  kernel_at <- if (d_max >= d_min) {
    steps <- d_min:d_max
    exp(steps * lambda - lgamma(steps + 1) - top)
  } else {
    numeric(0)
  }
  d <- outer(a2:b2, a:b, "-")
  step_matrix <- matrix(0, length(a2:b2), length(a:b))
  reachable <- d >= 0
  step_matrix[reachable] <- kernel_at[d[reachable] - d_min + 1]
  return(step_matrix)
}

#' One step of the forward pass over the cumulative counts
#'
#' Carries the log probabilities of the counts in one band to the counts in
#' the next, through the binomial step of the chain. The step is a matrix
#' product of [.meta_step_kernel()] with the exponentiated source vector,
#' each scaled so that the arithmetic stays in range, and a sum on the log
#' scale over every pair of counts where the kernel would underflow at the
#' most likely step. Both give the same probabilities. Matches
#' `meta_family_box_step()` in Stan.
#'
#' @param alpha The log probabilities of the source counts `a:b`.
#'
#' @param a,b The first and last source count.
#'
#' @param a2,b2 The first and last target count.
#'
#' @param r The step probability from [.meta_step_prob()].
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @param m_prev,m The most likely source and target counts from
#'  [.meta_box_mode_path()].
#'
#' @param lg The log factorials, `lg[d + 1]` being `log(d!)`.
#'
#' @returns The log probabilities of the target counts `a2:b2`.
#'
#' @keywords internal
.meta_box_step <- function(alpha, a, b, a2, b2, r, study_n, m_prev, m, lg) {
  n <- study_n
  state <- a:b
  target <- a2:b2
  # The floor keeps the log expected step inside the fixed point range.
  log_r <- log(max(r, 1e-200))
  log_1mr <- log(max(1 - r, 1e-200))
  lambda_scaled <- .meta_fixed_point(log_r + log(n - m_prev))
  if (.meta_step_kernel_alive(m - m_prev, lambda_scaled)) {
    lambda <- lambda_scaled / 2^20
    log_v <- alpha + lg[n - state + 1] - state * (log_r - lambda)
    shift <- max(log_v)
    if (!is.finite(shift)) {
      return(rep(-Inf, length(target)))
    }
    reached <- as.numeric(
      .meta_step_kernel(a, b, a2, b2, lambda_scaled) %*% exp(log_v - shift)
    )
    per_target <- target * (log_r - lambda) - lg[n - target + 1] +
      (n - target) * log_1mr + .meta_step_kernel_top(lambda_scaled)
    return(ifelse(
      reached >= 1e-300, log(reached) + shift + per_target, -Inf
    ))
  }
  # The coefficient of the step splits into a part in s, folded into alpha,
  # a part in t - s and a part in t, so each target count is one log sum
  # over the sources at or below it.
  folded <- alpha + lg[n - state + 1]
  d <- outer(target, state, "-")
  reachable <- d >= 0
  log_terms <- matrix(-Inf, length(target), length(state))
  log_terms[reachable] <- rep(folded, each = length(target))[reachable] +
    d[reachable] * log_r - lg[d[reachable] + 1]
  return(
    .meta_row_log_sum_exp(log_terms) + (n - target) * log_1mr -
      lg[n - target + 1]
  )
}

#' The joint log likelihood of several quantiles of integer day delays
#'
#' Each quantile reported at probability \eqn{p} and landing on day \eqn{y}
#' says that the empirical distribution function crossed \eqn{p} between the
#' day below and the day itself, \eqn{N_{\le y - w_s} \le \lceil n p \rceil
#' - 1} and \eqn{N_{\le y} \ge \lceil n p \rceil}, with \eqn{N_{\le y}} the
#' number of delays at or below \eqn{y}. The counts at the integer edges
#' the reported quantiles name form a Markov chain,
#' \deqn{N_{e_{i+1}} \mid N_{e_i} \sim N_{e_i} + \text{Binomial}\left(n -
#' N_{e_i}, \frac{G_0(e_{i+1}) - G_0(e_i)}{1 - G_0(e_i)}\right),}
#' with \eqn{G_0} the uncorrected grid distribution function, and the
#' likelihood is the probability that every count fell in its box. It is a
#' forward pass over the counts on the log scale, one step of
#' [.meta_box_step()] per edge, kept to a band of [.meta_band_half_width()]
#' counts around the most likely path of [.meta_box_mode_path()], so its
#' cost grows like the number of edges times the sample size to the power
#' one and a half rather than squared. Two quantiles reported at the same
#' value are two constraints at one edge, and reporting the same count at
#' two values is a box no chain can satisfy, which gives `-Inf`.
#'
#' This is the joint form of [.meta_grid_crossing_ll()], which it reduces
#' to for a single quantile. Like it, the information it carries saturates
#' as the study grows, and at a thousand delays it is close to an indicator
#' of the parameters that put the population quantiles in the reported
#' cells, a box rather than a peak. The multinomial of
#' [.meta_quantile_set_ll()] on the continuity corrected grid keeps
#' sharpening with the sample size instead, so it is not used for such a
#' study.
#'
#' Matches `meta_family_grid_box_ll()` in Stan.
#'
#' @param y A vector of reported quantile values in non decreasing order.
#'
#' @param upper The largest count of delays below each reported day.
#'
#' @param lower The smallest count of delays at or below each reported day.
#'
#' @inheritParams .meta_quantile_set_ll
#'
#' @returns A log probability mass.
#'
#' @keywords internal
.meta_grid_box_ll <- function(y, upper, lower, study_n, dist, args, slots) {
  y <- y - .meta_cens_shift(slots$cens_adjusted, slots$pwindow, slots$swindow)
  accrual <- .meta_accrual_flag(slots$trunc_adjusted, slots$trunc_design)
  n_grid <- floor(slots$cutoff / slots$swindow)
  first <- .meta_grid_first(slots$lower, slots$swindow)
  cell <- floor(y / slots$swindow + 0.5)
  if (any(cell < first | cell >= n_grid)) {
    return(-Inf)
  }
  # Every reported day names two edges, the day below it with an upper
  # bound and the day itself with a lower bound. An edge below the first
  # cell holds no delays, so its bound is met whenever it is not negative.
  edge <- c(cell - 1, cell)
  box_lower <- c(rep(0, length(cell)), lower)
  box_upper <- c(upper, rep(study_n, length(cell)))
  if (any(box_upper[edge < first] < 0)) {
    return(-Inf)
  }
  keep <- edge >= first
  edges <- sort(unique(edge[keep]))
  lo <- vapply(
    edges, function(e) max(box_lower[keep][edge[keep] == e]), numeric(1)
  )
  hi <- vapply(
    edges, function(e) min(box_upper[keep][edge[keep] == e]), numeric(1)
  )
  if (any(lo > hi)) {
    return(-Inf)
  }
  cdf <- .meta_grid_cdf(
    edges, dist, args, slots$lower, slots$cutoff, slots$pwindow,
    slots$swindow, slots$growth_rate, accrual
  )
  if (!all(is.finite(cdf))) {
    return(-Inf)
  }
  cdf <- cummax(pmin(pmax(cdf, 0), 1))
  n <- study_n
  stride <- .meta_box_stride(n)
  path <- .meta_box_mode_path(n, cdf, lo, hi, stride)
  if (is.null(path)) {
    return(-Inf)
  }
  band <- vapply(
    seq_along(edges),
    function(i) {
      h <- .meta_band_half_width(path[i] * (1 - path[i] / n), stride)
      return(c(max(lo[i], path[i] - h), min(hi[i], path[i] + h)))
    },
    numeric(2)
  )
  # lg[d + 1] is log d!, so that the binomial coefficients of every step are
  # a lookup rather than a special function call.
  lg <- lgamma(seq_len(n + 1))
  alpha <- .meta_log_binom(band[1, 1]:band[2, 1], n, cdf[1])
  for (i in seq_along(edges)[-1]) {
    alpha <- .meta_box_step(
      alpha, band[1, i - 1], band[2, i - 1], band[1, i], band[2, i],
      .meta_step_prob(cdf[i - 1], cdf[i]), n, path[i - 1], path[i], lg
    )
  }
  return(.meta_log_sum_exp(alpha))
}

#' The joint log likelihood of a set of quantiles from one study
#'
#' Quantiles reported at probabilities \eqn{p_1 < \dots < p_k} with values
#' \eqn{y_1 \le \dots \le y_k} split the delay axis into the cells
#' \eqn{(0, y_1], \dots, (y_{k-1}, y_k], (y_k, \infty)}, and the number of
#' delays falling in each cell is multinomial with probabilities given by the
#' increments of the implied distribution function. This is the joint version
#' of the empirical distribution function likelihood used for a single
#' quantile, and it reduces to the exact binomial when only one quantile of a
#' continuous estimand is reported. Fitting each quantile separately ignores
#' the positive correlation between the empirical distribution function at
#' different points, which over weights a study reporting a median with an
#' interquartile range.
#'
#' Two quantiles reported at the same value are two constraints on the
#' empirical distribution function at one cell, so they are merged into that
#' cell with their combined count.
#'
#' Quantiles of integer day delays (`cens_adjusted` 0 or 3) are discrete
#' statistics, and the multinomial on the continuity corrected distribution
#' function keeps sharpening with the study size while a rounded quantile
#' stops moving. A single such quantile is fitted by
#' [.meta_grid_crossing_ll()] as the cell in which the empirical
#' distribution function crossed its probability, and several by
#' [.meta_grid_box_ll()] as the joint probability of every such crossing,
#' with `cum_count` and `lower` read as the box each crossing puts on the
#' counts below and at the reported day.
#'
#' A cell whose implied probability underflows to zero while the study saw
#' delays in it is floored at [.meta_cell_floor()].
#'
#' @param y A vector of reported quantile values in non decreasing order.
#'
#' @param cum_count A vector of cumulative counts from
#'  [.meta_quantile_counts()], or for quantiles of integer day delays the
#'  largest count of delays below each reported day, from
#'  [.meta_crossing_counts()] less one.
#'
#' @param lower For quantiles of integer day delays, the smallest count of
#'  delays at or below each reported day, from [.meta_crossing_counts()].
#'  Unused otherwise.
#'
#' @param study_n The number of delays the quantiles were computed from.
#'
#' @param dist A `primarycensored` distribution function name.
#'
#' @param args A named list of distribution parameters.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @param p The probabilities the quantiles were reported at, in the order
#'  of `y`. Only used for a single quantile of integer day delays.
#'
#' @returns A log probability mass.
#'
#' @keywords internal
.meta_quantile_set_ll <- function(
  y,
  cum_count,
  study_n,
  dist,
  args,
  slots,
  p = slots$group_p,
  lower = slots$group_lower
) {
  if (slots$cens_adjusted %in% c(0, 3)) {
    if (length(y) == 1) {
      return(.meta_grid_crossing_ll(y, p[1], study_n, dist, args, slots))
    }
    return(.meta_grid_box_ll(
      y, cum_count, lower, study_n, dist, args, slots
    ))
  }
  # Matches the skip over coincident values in the Stan mirror.
  keep <- !duplicated(y, fromLast = TRUE)
  y <- y[keep]
  cum_count <- cum_count[keep]
  prob <- .meta_implied_probs(y, dist, args, slots)
  if (!all(is.finite(prob))) {
    return(-Inf)
  }
  cell <- pmax(diff(c(0, prob, 1)), .meta_cell_floor())
  count <- diff(c(0, cum_count, study_n))
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
#' delay, as studies report it, so such a row is fitted on that scale against
#' the implied quantile of [.meta_node_quantile()]. A quantile row without a
#' standard error is fitted on the cumulative probability scale, where the
#' binomial standard error of an empirical distribution function applies.
#'
#' A group row stands for several summaries reported by one study, and this
#' returns the marginal of its first member, which is the reported mean of a
#' mean and standard deviation pair and the cumulative probability at the
#' smallest reported quantile of a quantile set. For a group covered by a
#' covariance matrix it is the first element of the reported vector, with the
#' first diagonal entry of the Cholesky factor as its standard error, so a
#' posterior predictive check of such a row describes that element alone and
#' not the rest of the group. That marginal is what the posterior predictive
#' draws for the row. The joint log likelihood of the whole group is
#' [.meta_row_log_lik()].
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
  if (slots$obs_type == 8L) {
    pieces <- .meta_joint_study_terms(dist, args, slots, moments)
    if (is.null(pieces)) {
      return(c(observed = slots$group_value[1], implied = Inf, se = Inf))
    }
    return(c(
      observed = slots$group_value[1],
      implied = unname(pieces$implied[1]),
      se = sqrt(pieces$sigma[1, 1])
    ))
  }
  if (slots$obs_type == 4L && slots$report_se > 0) {
    # Studies report a quantile's standard error on the delay scale, so the
    # reported value is compared with the implied quantile on that scale.
    # Converting the standard error to the probability scale with the density
    # at the reported value collapses far from the implied quantile, turning
    # a discrepant row into a wall rather than a slope.
    nodes <- .meta_implied_nodes(dist, args, slots)
    implied <- .meta_node_quantile(nodes, slots$quantile_p, dist, args, slots)
    if (is.na(implied)) {
      implied <- Inf
    }
    return(c(
      observed = slots$value,
      implied = implied,
      se = max(slots$report_se, .meta_min_prob_se())
    ))
  }
  if (slots$obs_type %in% c(4L, 6L)) {
    implied <- .meta_implied_prob(
      slots$value, dist, args, slots$lower, slots$cutoff, slots$pwindow,
      slots$swindow, slots$trunc_adjusted, slots$cens_adjusted,
      slots$growth_rate, slots$trunc_design, .meta_slots_n_quad(slots)
    )
    observed <- slots$quantile_p
    se <- sqrt(slots$quantile_p * (1 - slots$quantile_p) / slots$study_n)
  } else {
    if (is.null(moments)) {
      moments <- .meta_row_moments(slots, dist, args)
    }
    observed <- slots$value
    if (!all(is.finite(moments))) {
      # The same rejection as .meta_row_log_lik(), so that a draw whose
      # moments overflow predicts nothing rather than a NaN standard error.
      return(c(observed = unname(observed), implied = Inf, se = Inf))
    }
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
#' a standard deviation, [.meta_quantile_set_ll()] for a set of quantiles,
#' which hands quantiles of integer day delays to [.meta_grid_crossing_ll()]
#' or [.meta_grid_box_ll()], and [.meta_joint_study_ll()] for a continuous
#' estimand reporting both kinds.
#'
#' A draw whose implied moments are not all finite, which an extreme delay
#' distribution parameter can produce by overflowing the analytic kurtosis,
#' is rejected with a log likelihood of `-Inf` rather than `NaN`, for every
#' row that uses the moments. Matches the guard in `meta_family_lpmf` in
#' `inst/stan/meta_model/functions.stan`.
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
  if (slots$obs_type == 8L) {
    return(.meta_joint_study_ll(
      slots$group_value, dist, args, slots, moments
    ))
  }
  if (slots$obs_type %in% c(2L, 3L, 5L)) {
    if (is.null(moments)) {
      moments <- .meta_row_moments(slots, dist, args)
    }
    if (!all(is.finite(moments))) {
      return(-Inf)
    }
  }
  if (slots$obs_type == 5L) {
    return(.meta_moment_pair_ll(
      slots$group_value[1], slots$group_value[2], moments, slots$study_n
    ))
  }
  if (slots$obs_type == 6L) {
    return(.meta_quantile_set_ll(
      slots$group_value, slots$group_count, slots$study_n, dist, args, slots,
      p = slots$group_p, lower = slots$group_lower
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
    slots$trunc_design, .meta_slots_n_quad(slots)
  ))
}

#' The implied summaries of one meta model row for every posterior draw
#'
#' Rows sharing a study design, the same parameter draws and the same
#' quadrature resolution imply the same summaries, so they are computed once
#' and reused. The cache is bounded and lives in the package namespace, so it
#' is never written into a fitted model object. See [.meta_draws]. A row with
#' an estimated growth rate holds one rate per draw, so the rates are part of
#' what a cached entry is compared against, and each draw is summarised at
#' its own rate.
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
  needs_moments <- slots$obs_type %in% c(2L, 3L, 5L, 8L) ||
    (slots$obs_type == 7L && any(slots$group_type != 3L))
  if (!needs_moments) {
    return(vector("list", length(dist_args)))
  }
  # Every field of the design is written out in full, so two different designs
  # cannot share a key. The quadrature resolution is part of the key as well,
  # because changing it changes the summaries a design implies.
  key <- paste(
    dist, length(dist_args), .meta_slots_n_quad(slots), slots$trunc_adjusted,
    slots$cens_adjusted, slots$trunc_design, slots$growth_known,
    sprintf(
      "%.17g|%.17g|%.17g|%.17g|%.17g",
      slots$lower, slots$cutoff, slots$pwindow, slots$swindow,
      slots$growth_rate[1]
    ),
    sep = "|"
  )
  cached <- .meta_draws[[key]]
  if (
    !is.null(cached) && identical(cached$args, dist_args) &&
      identical(cached$growth, slots$growth_rate)
  ) {
    return(cached$moments)
  }
  moments <- lapply(seq_along(dist_args), function(draw) {
    return(.meta_row_moments(
      .meta_draw_slots(slots, draw), dist, dist_args[[draw]]
    ))
  })
  if (length(.meta_draws) >= .meta_draw_cache_limit()) {
    rm(list = ls(.meta_draws), envir = .meta_draws)
  }
  assign(
    key,
    list(args = dist_args, growth = slots$growth_rate, moments = moments),
    envir = .meta_draws
  )
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
        .meta_draw_slots(slots, draw), dist_name, dist_args[[draw]],
        moments[[draw]]
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
        .meta_draw_slots(slots, draw), dist_name, dist_args[[draw]],
        moments[[draw]]
      )
      # A draw the likelihood rejects has no predictive distribution.
      if (!is.finite(summaries[["se"]])) {
        return(NA_real_)
      }
      return(stats::rnorm(1, summaries[["implied"]], summaries[["se"]]))
    })
    return(as.matrix(draws))
  }

  return(.predict)
}
