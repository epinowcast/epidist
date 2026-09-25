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
#' Both start the grid at `delay_min` moved back by [.meta_cens_lower()], as
#' the mean and standard deviation of the same study do. For a midpoint
#' imputed grid (`cens_adjusted` 3) the reported `delay_min` sits on the
#' midpointed scale, so an off grid value would otherwise drop the lowest
#' counted cell.
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
    # The grid starts from delay_min moved back to the base estimand, as it
    # does for the same study's mean and standard deviation. The censoring
    # code is kept so that the reported values move back as well.
    slots$lower <- .meta_cens_lower(
      slots$lower, slots$cens_adjusted, slots$pwindow, slots$swindow
    )
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
