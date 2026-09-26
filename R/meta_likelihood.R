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
      return(.meta_quantile_covariance(
        probs[i], probs[j], density[position[i]], density[position[j]],
        study_n
      ))
    }
    if (types[i] == 3L) {
      return(entry(j, i))
    }
    if (types[j] == 3L) {
      m <- position[j]
      return(.meta_cross_covariance(
        types[i], probs[j], density[m], partial[, m], spread, study_n
      ))
    }
    return(.meta_moment_covariance(types[i], types[j], se_mean, se_sd, rho))
  }
  covariance <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      covariance[i, j] <- entry(i, j)
    }
  }
  return(covariance)
}

#' The entries of the joint sampling covariance of one study's summaries
#'
#' `.meta_quantile_covariance()` gives the Bahadur covariance of two
#' quantiles, `.meta_cross_covariance()` that of a mean or a
#' standard deviation and a quantile, and `.meta_moment_covariance()` that of
#' two moment summaries, see [.meta_joint_covariance()]. They mirror the Stan
#' functions `meta_family_quantile_covariance()`,
#' `meta_family_cross_covariance()` and
#' `meta_family_moment_covariance()` in `inst/stan/meta_model/functions.stan`.
#'
#' @param prob_i,prob_j,prob Member probabilities.
#'
#' @param density_i,density_j,density The implied density at the implied
#'  quantiles.
#'
#' @param moment_type,type_i,type_j Member types, 1 for a mean and 2 for a
#'  standard deviation.
#'
#' @param partial The two centred partial moments at the quantile, a column
#'  of [.meta_quantile_partials()].
#'
#' @param spread The implied standard deviation.
#'
#' @param se_mean,se_sd The sampling standard errors of the mean and the
#'  standard deviation.
#'
#' @param rho The sampling correlation of the mean and the standard deviation
#'  from [.meta_moment_correlation()].
#'
#' @inheritParams .meta_joint_covariance
#'
#' @returns A covariance.
#'
#' @keywords internal
.meta_quantile_covariance <- function(
  prob_i,
  prob_j,
  density_i,
  density_j,
  study_n
) {
  return(
    min(prob_i, prob_j) * (1 - max(prob_i, prob_j)) /
      (study_n * density_i * density_j)
  )
}

#' @rdname dot-meta_quantile_covariance
#' @keywords internal
.meta_cross_covariance <- function(
  moment_type,
  prob,
  density,
  partial,
  spread,
  study_n
) {
  if (moment_type == 1L) {
    return(-partial[1] / (study_n * density))
  }
  return(
    -(partial[2] - spread^2 * prob) / (2 * spread * study_n * density)
  )
}

#' @rdname dot-meta_quantile_covariance
#' @keywords internal
.meta_moment_covariance <- function(type_i, type_j, se_mean, se_sd, rho) {
  if (type_i != type_j) {
    return(rho * se_mean * se_sd)
  }
  if (type_i == 1L) {
    return(se_mean^2)
  }
  return(se_sd^2)
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
