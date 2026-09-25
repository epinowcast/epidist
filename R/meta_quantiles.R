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
      .qdist(dist), c(list(p = base + p * (top - base)), args)
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
