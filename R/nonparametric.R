#' Non-parametric delay distribution family
#'
#' @description
#' A delay distribution with no parametric form, for use with the marginal
#' and meta models. Pass it as the `family` argument of [epidist()] in the
#' same way as [brms::lognormal()].
#'
#' The delay sits on a grid of \eqn{K} bins with boundaries
#' \eqn{b_0 < b_1 < \dots < b_K}. All the probability of bin \eqn{i} is
#' placed at its right edge \eqn{b_i}, so the delay distribution function is
#' a step function. This is the discrete hazard distribution of
#' [primarycensored::pdiscretehazard()], and the likelihood is the
#' `primarycensored` one for that distribution.
#'
#' The distribution is written in terms of the discrete time hazard of each
#' bin, the probability that the delay ends in bin \eqn{i} given that it did
#' not end before it. The hazard of the last bin is 1, so all the delays end
#' by \eqn{b_K}. The other hazards are modelled on the logit scale as
#' \deqn{\mathrm{logit}(h_i) = \mu + \sigma \delta_i.}
#'
#' `mu` is the distributional parameter that takes a formula. A covariate in
#' the `mu` formula shifts the logit hazard of every bin by the same amount,
#' which is a proportional odds model for the hazard. `hsigma` is
#' \eqn{\sigma}, the spread of the bin offsets \eqn{\delta_i}. The offsets are
#' built from standard normal innovations, one distributional parameter per
#' innovation, named `h<i>eps` after the bin they enter at.
#'
#' * `hazard_model = "rw"`, the default, is a random walk on the logit
#'   hazard: \eqn{\delta_1 = 0} and \eqn{\delta_i = \sum_{j = 2}^{i}
#'   \epsilon_j}. Neighbouring bins have similar hazards, so the fitted
#'   distribution is smooth.
#' * `hazard_model = "re"` treats the logit hazards as independent random
#'   effects around `mu`: \eqn{\delta_i = \epsilon_i}. It does not borrow
#'   strength between neighbouring bins.
#'
#' The random walk is the default because `primarycensored` recommends it as
#' the better starting point for most delays. See the `primarycensored`
#' article on fitting non-parametric delays at
#' <https://primarycensored.epinowcast.org/articles/fitting-nonparametric-delays.html>.
#'
#' The default priors are `normal(0, 1.5)` on the intercept of `mu`, which is
#' close to uniform on the hazard scale, `normal(0, 1)` on the intercept of
#' `hsigma` on the log scale, as in `primarycensored`, and `std_normal()` on
#' each innovation. Set others with the `prior` argument of [epidist()].
#'
#' @param boundaries A numeric vector of at least four strictly increasing
#'  bin boundaries, \eqn{b_0} to \eqn{b_K}. The default, `NULL`, is set by
#'  [epidist()] from the data, with a bin for every whole delay from 0 up to
#'  the longest delay in the data, that is `seq(-1, max_delay)`. The last
#'  boundary must be at least as long as the longest observed delay.
#'
#' @param hazard_model The model for the logit hazards, `"rw"` for a random
#'  walk, the default, or `"re"` for independent random effects.
#'
#' @returns A `brmsfamily` object for use as the `family` argument of
#'  [epidist()].
#'
#' @family family
#' @export
#' @examples
#' nonparametric(boundaries = -1:10)
#' nonparametric(hazard_model = "re")
nonparametric <- function(boundaries = NULL, hazard_model = c("rw", "re")) {
  hazard_model <- match.arg(hazard_model)
  family <- list(
    family = paste0("discretehazard_", hazard_model),
    link = "identity",
    dpars = "mu",
    ybounds = c(-Inf, Inf),
    np = list(boundaries = NULL, hazard_model = hazard_model)
  )
  class(family) <- c("brmsfamily", "family")
  if (!is.null(boundaries)) {
    family <- .np_set_boundaries(family, boundaries)
  }
  return(family)
}

#' Is a family the non-parametric hazard family?
#'
#' @param family A family object, or a list recording a delay family as
#'  returned by `.delay_family()`.
#'
#' @returns A logical scalar.
#'
#' @keywords internal
.is_nonparametric <- function(family) {
  return(is.list(family) && !is.null(family$np))
}

#' Check the bin boundaries of the non-parametric family
#'
#' @inheritParams nonparametric
#'
#' @returns The boundaries as a numeric vector, invisibly.
#'
#' @keywords internal
.assert_np_boundaries <- function(boundaries) {
  assert_numeric(boundaries, any.missing = FALSE, finite = TRUE)
  if (length(boundaries) < 4) {
    cli_abort(c(
      "{.arg boundaries} must hold at least four values, so at least three
       bins.",
      i = "It holds {length(boundaries)}."
    ))
  }
  if (any(diff(boundaries) <= 0)) {
    cli_abort("{.arg boundaries} must be strictly increasing.")
  }
  return(invisible(as.numeric(boundaries)))
}

#' Set the bin boundaries of the non-parametric family
#'
#' Fixes the number of bins, and so the distributional parameters, their
#' links and their bounds.
#'
#' @param family A family built by [nonparametric()].
#'
#' @inheritParams nonparametric
#'
#' @returns The family with its boundaries and parameters set.
#'
#' @keywords internal
.np_set_boundaries <- function(family, boundaries) {
  boundaries <- .assert_np_boundaries(boundaries)
  eps <- .np_eps_names(length(boundaries) - 1, family$np$hazard_model)
  family$np$boundaries <- boundaries
  family$dpars <- c("mu", "hsigma", eps)
  family$other_links <- c("log", rep("identity", length(eps)))
  family$other_bounds <- c(
    list(list(lb = "0", ub = "")),
    rep(list(list(lb = "", ub = "")), length(eps))
  )
  return(family)
}

#' Names of the innovation parameters of the non-parametric family
#'
#' `brms` reads a trailing digit in a distributional parameter name as a
#' mixture component, so each name puts the bin index before `eps`.
#'
#' @param n_bins The number of bins, \eqn{K}.
#'
#' @inheritParams nonparametric
#'
#' @returns A character vector of parameter names.
#'
#' @keywords internal
.np_eps_names <- function(n_bins, hazard_model) {
  first <- ifelse(hazard_model == "rw", 2L, 1L)
  return(paste0("h", seq(first, n_bins - 1L), "eps"))
}

#' Resolve the non-parametric family against the model data
#'
#' Checks that the model supports the family, sets the default boundaries
#' from the data where none were given, and checks that the boundaries reach
#' the longest observed delay.
#'
#' @inheritParams epidist_family
#'
#' @param family A family built by [nonparametric()].
#'
#' @returns The family with its boundaries set.
#'
#' @keywords internal
.np_resolve <- function(family, data) {
  if (!inherits(data, c("epidist_marginal_model", "epidist_meta_model"))) {
    cli_abort(c(
      "The {.fn nonparametric} family is only supported by the marginal and
       meta models.",
      i = "Use {.fn as_epidist_marginal_model} or
           {.fn as_epidist_meta_model}."
    ))
  }
  longest <- .np_longest_delay(data)
  if (is.null(family$np$boundaries)) {
    if (!is.finite(longest)) {
      cli_abort(c(
        "Could not set default {.arg boundaries} for {.fn nonparametric}
         because the data hold no finite delay.",
        i = "Pass {.arg boundaries} to {.fn nonparametric}."
      ))
    }
    family <- .np_set_boundaries(family, seq(-1, ceiling(longest)))
  }
  top <- max(family$np$boundaries)
  if (is.finite(longest) && top < .np_longest_observed(data)) {
    cli_abort(c(
      "The last of {.arg boundaries} must be at least the longest observed
       delay.",
      i = "It is {top} and the longest delay is
           {.np_longest_observed(data)}."
    ))
  }
  return(family)
}

#' The longest delay observed in individual level data
#'
#' @inheritParams epidist_family
#'
#' @returns The largest lower delay bound of an individual level row, or
#'  `-Inf` where there are none.
#'
#' @keywords internal
.np_longest_observed <- function(data) {
  rows <- .np_individual_rows(data)
  if (!any(rows)) {
    return(-Inf)
  }
  return(max(data$delay_lwr[rows]))
}

#' The longest delay the data could hold
#'
#' The upper delay bound of the individual level rows, and for a meta model
#' the finite observation time of each summary row, which bounds the delays
#' the study saw.
#'
#' @inheritParams epidist_family
#'
#' @returns A number, `-Inf` where the data hold no finite delay.
#'
#' @keywords internal
.np_longest_delay <- function(data) {
  rows <- .np_individual_rows(data)
  delays <- data$delay_upr[rows]
  if (inherits(data, "epidist_meta_model")) {
    delays <- c(delays, data$relative_obs_time[!rows])
  }
  delays <- delays[is.finite(delays)]
  if (length(delays) == 0) {
    return(-Inf)
  }
  return(max(delays))
}

#' Which rows of the model data are individual level delays
#'
#' @inheritParams epidist_family
#'
#' @returns A logical vector.
#'
#' @keywords internal
.np_individual_rows <- function(data) {
  if (inherits(data, "epidist_meta_model")) {
    return(data$obs_type == 1L)
  }
  return(rep(TRUE, nrow(data)))
}

#' The innovation parameters of the non-parametric family
#'
#' @param dpars A character vector of distributional parameter names.
#'
#' @returns The names of the innovation parameters, in bin order.
#'
#' @keywords internal
.np_eps_dpars <- function(dpars) {
  eps <- grep("^h[0-9]+eps$", dpars, value = TRUE)
  index <- as.integer(gsub("[^0-9]", "", eps))
  return(eps[order(index)])
}

#' Logit hazard offsets of the non-parametric family
#'
#' @param dpars A named list of distributional parameter vectors of equal
#'  length, holding `hsigma` and the innovations.
#'
#' @inheritParams nonparametric
#'
#' @returns A matrix with one row per element of the vectors and one column
#'  per bin but the last.
#'
#' @keywords internal
.np_offsets <- function(dpars, hazard_model) {
  eps_names <- .np_eps_dpars(names(dpars))
  n <- length(dpars$mu)
  eps <- matrix(
    unlist(lapply(dpars[eps_names], rep_len, length.out = n)),
    nrow = n
  )
  if (hazard_model == "rw") {
    eps <- cbind(0, eps)
    for (j in seq_len(ncol(eps))[-1]) {
      eps[, j] <- eps[, j - 1] + eps[, j]
    }
  }
  return(eps * rep_len(dpars$hsigma, n))
}

#' Bin probabilities of the non-parametric family
#'
#' Each row is the probability mass at the right edge of each bin, from the
#' hazards \eqn{h_i} as \eqn{h_i \prod_{j < i} (1 - h_j)}. Matches
#' [primarycensored::hazards_to_pmf()].
#'
#' @inheritParams .np_offsets
#'
#' @param boundaries The bin boundaries.
#'
#' @returns A matrix with one row per element of the vectors in `dpars` and
#'  one column per bin.
#'
#' @keywords internal
.np_pmf <- function(dpars, boundaries, hazard_model) {
  hazards <- .np_hazards(dpars, hazard_model)
  n_bins <- ncol(hazards)
  log_surv <- matrix(0, nrow = nrow(hazards), ncol = n_bins)
  for (j in seq_len(n_bins)[-1]) {
    log_surv[, j] <- log_surv[, j - 1] + log1p(-hazards[, j - 1])
  }
  return(hazards * exp(log_surv))
}

#' Bin hazards of the non-parametric family
#'
#' @inheritParams .np_offsets
#'
#' @returns A matrix with one row per element of the vectors in `dpars` and
#'  one column per bin, whose last column is 1.
#'
#' @keywords internal
.np_hazards <- function(dpars, hazard_model) {
  n <- length(dpars$mu)
  logit <- rep_len(dpars$mu, n) + .np_offsets(dpars, hazard_model)
  return(cbind(stats::plogis(logit), 1))
}

#' Distribution parameters of the non-parametric family for each draw
#'
#' @param prep A `brms` prepared predictions object.
#'
#' @param i The index of the observation.
#'
#' @param np The `np` element of the family, holding the boundaries and the
#'  hazard model.
#'
#' @returns A list with one element per draw, each a list of `boundaries`
#'  and `hazards` for [primarycensored::pdiscretehazard()].
#'
#' @keywords internal
.np_dist_args <- function(prep, i, np) {
  dpar_names <- c("mu", "hsigma", .np_eps_dpars(names(prep$dpars)))
  dpars <- lapply(dpar_names, function(dpar) {
    return(brms::get_dpar(prep, dpar, i = i))
  })
  names(dpars) <- dpar_names
  hazards <- .np_hazards(dpars, np$hazard_model)
  return(lapply(seq_len(nrow(hazards)), function(draw) {
    return(list(boundaries = np$boundaries, hazards = hazards[draw, ]))
  }))
}
