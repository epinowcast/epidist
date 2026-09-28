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
    height <- do.call(.ddist(dist), c(list(x = y), args))
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
