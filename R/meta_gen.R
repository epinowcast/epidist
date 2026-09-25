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
#' Summary rows are evaluated in R, one posterior draw at a time, and each
#' evaluation can run a quadrature of up to 2000 intervals by default. The
#' cost of [brms::log_lik()] and [loo::loo()] therefore grows with the number
#' of draws times the number of summary rows. Pass `ndraws` to either to use
#' fewer draws, at the price of noisier estimates.
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
