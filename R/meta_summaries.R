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
