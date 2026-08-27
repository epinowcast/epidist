#' Create a function to calculate the marginalised log likelihood for double
#' censored and truncated delay distributions
#'
#' This function creates a log likelihood function that calculates the marginal
#' likelihood for a single observation by integrating over the latent primary
#' and secondary event windows. Where analytical solutions exist in
#' [primarycensored::dpcens()] these are used, otherwise the integration is
#' performed numerically. [primarycensored::dpcens()] handles the double
#' censoring and truncation of the delay distribution.
#'
#' The marginal likelihood accounts for uncertainty in both the primary and
#' secondary event windows by integrating over their possible values, weighted
#' by their respective uniform distributions.
#'
#' @seealso [brms::log_lik()] for details on the brms log likelihood interface.
#'
#' @inheritParams epidist_family
#'
#' @return A function that calculates the marginal log likelihood for a single
#' observation. The prep object must have the following variables:
#' * `vreal1`: relative observation time
#' * `vreal2`: primary event window
#' * `vreal3`: secondary event window
#' * `vreal4`: delay upper bound
#' * `vreal5`: minimum delay (left truncation point; defaults to 0 if absent)
#'
#' @family gen
#' @autoglobal
#' @importFrom purrr map_dbl
#' @export
epidist_gen_log_lik <- function(family) {
  # Get internal brms log_lik function
  log_lik_brms <- .get_brms_fn("log_lik", family)

  # Get the name of the primary distribution
  primary_dist_name <- .pcd_family_dist_name(family)

  # Check if family is supported with a analytical solution
  if (primary_dist_name %in% .get_supported_dists()) {
    .log_lik <- .analytical_gen_log_lik(primary_dist_name)
  } else {
    cli::cli_inform(
      c(
        "Falling back to default dependency on brms for {primary_dist_name}",
        "distribution when generating the log likelihood in R. To improve",
        "performance, implement a .get_supported_dist_args_{primary_dist_name}",
        "function and ensure that p{primary_dist_name} is an available",
        "function."
      ),
      .frequency = "once",
      .frequency_id = paste0("epidist_gen_log_lik_", primary_dist_name)
    )
    .log_lik <- .generic_gen_log_lik(log_lik_brms)
  }

  return(.log_lik)
}

.generic_gen_log_lik <- function(log_lik_brms) {
  .log_lik <- function(i, prep) {
    y <- prep$data$Y[i]
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]
    delay_min <- if (is.null(prep$data$vreal5)) 0 else prep$data$vreal5[i]

    # make the prep object censored
    # -1 here is equivalent to right censored in brms
    # this means we get the cdf of the target distribution
    prep$data$cens <- rep(-1, prep$nobs)

    # This integrates with pcens_cdf() rather than dpcens(), so the guard
    # dpcens() applies has to be reproduced here. Without it the truncation
    # normalisation below divides by a cdf evaluated before the delay upper
    # bound, which can push the density above one.
    if (y + swindow > relative_obs_time) {
      cli::cli_abort(
        c(
          "Upper delay bound is greater than the relative observation time.",
          i = "For observation {i} it is {y + swindow} and the relative
               observation time is {relative_obs_time}."
        )
      )
    }

    # A single call to the brms log likelihood returns the cdf at one delay
    # for every draw, so cache those columns by delay and evaluate each delay
    # once instead of once per draw. The numerical integration below uses the
    # same quadrature nodes for each draw, so after the first draw the
    # lookups are cache hits.
    cache <- new.env(parent = emptyenv())
    cache$delays <- numeric(0)
    cache$cdf <- NULL
    cache$draw <- 1L

    fill_cache <- function(q) {
      new_delays <- unique(q[!(q %in% cache$delays)])
      if (length(new_delays) > 0) {
        new_cdf <- lapply(new_delays, function(x) {
          prep_x <- prep
          prep_x$data$Y <- rep(x, length(prep$data$Y))
          prep_x$data$weights <- NULL
          return(exp(log_lik_brms(i, prep_x)))
        })
        cache$delays <- c(cache$delays, new_delays)
        cache$cdf <- cbind(cache$cdf, do.call(cbind, new_cdf))
      }
      return(match(q, cache$delays))
    }

    pdist_draw <- function(q, ...) {
      # `fill_cache()` grows `cache$cdf`, so resolve the index first.
      idx <- fill_cache(q)
      return(cache$cdf[cache$draw, idx])
    }

    # [primarycensored::dpcens()] revalidates `pdist` at random points on
    # every call, which would miss the cache once per draw, so integrate with
    # [primarycensored::pcens_cdf()] and form the censored pmf here.
    pcens_obj <- primarycensored::new_pcens(
      pdist = pdist_draw,
      dprimary = stats::dunif,
      dprimary_args = list()
    )
    delays <- unique(c(y, y + swindow, relative_obs_time, delay_min))
    delays <- sort(delays[is.finite(delays)])
    upr <- match(y + swindow, delays)
    lwr <- match(y, delays)
    trunc_idx <- match(relative_obs_time, delays)
    min_idx <- match(delay_min, delays)

    lpdf <- purrr::map_dbl(seq_len(prep$ndraws), function(d) {
      cache$draw <- d
      cdfs <- primarycensored::pcens_cdf(pcens_obj, delays, pwindow)
      pmf <- cdfs[upr] - cdfs[lwr]
      # Normalise over the interval that could have been observed, which runs
      # from the left truncation point to the relative observation time, as
      # [primarycensored::dpcens()] does.
      lower_mass <- if (is.na(min_idx)) 0 else cdfs[min_idx]
      if (!is.na(trunc_idx)) {
        pmf <- pmf / (cdfs[trunc_idx] - lower_mass)
      }
      return(log(max(0, pmf)))
    })
    lpdf <- .log_lik_weight(lpdf, i = i, prep = prep)
    return(lpdf)
  }

  return(.log_lik)
}

.analytical_gen_log_lik <- function(dist) {
  .log_lik <- function(i, prep) {
    y <- prep$data$Y[i]
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]
    delay_min <- if (is.null(prep$data$vreal5)) 0 else prep$data$vreal5[i]

    # Get distribution-specific parameters
    dist_args <- .get_supported_dist_args(dist, prep, i)

    # Calculate density for each draw using primarycensored::dpcens()
    lpdf <- purrr::map_dbl(seq_len(prep$ndraws), function(draw) {
      return(
        do.call(
          primarycensored::dpcens,
          c(
            list(
              x = y,
              pdist = .pdist(dist),
              pwindow = pwindow,
              swindow = swindow,
              L = delay_min,
              D = relative_obs_time,
              dprimary = stats::dunif,
              log = TRUE
            ),
            dist_args[[draw]]
          )
        )
      )
    })
    lpdf <- .log_lik_weight(lpdf, i = i, prep = prep)
    return(lpdf)
  }

  return(.log_lik)
}

# Helper to get distribution-specific arguments
.get_supported_dist_args <- function(dist, prep, i) {
  dist_params <- switch(dist,
    pgamma = {
      shape <- brms::get_dpar(prep, "shape", i = i)
      list(shape = shape, scale = brms::get_dpar(prep, "mu", i) / shape)
    },
    plnorm = {
      list(
        meanlog = brms::get_dpar(prep, "mu", i),
        sdlog = brms::get_dpar(prep, "sigma", i = i)
      )
    },
    pweibull = {
      shape <- brms::get_dpar(prep, "shape", i = i)
      list(
        shape = shape,
        scale = brms::get_dpar(prep, "mu", i = i) / gamma(1 + 1 / shape)
      )
    }
  )
  return(.transpose_named_list2(dist_params))
}

#' The `primarycensored` distribution name for a family
#'
#' Falls back to the lower cased family name if `primarycensored` does not
#' recognise it, so the caller can still report a name in a message.
#'
#' @inheritParams epidist_family
#'
#' @returns A `primarycensored` distribution function name, for example
#'  `"plnorm"`.
#'
#' @keywords internal
.pcd_family_dist_name <- function(family) {
  return(tryCatch(
    primarycensored::pcd_dist_name(tolower(family$family)),
    error = function(e) tolower(family$family)
  ))
}

#' The distribution function used for a `primarycensored` distribution name
#'
#' @param dist A `primarycensored` distribution function name, for example
#'  `"plnorm"`.
#'
#' @returns The corresponding function from `stats`.
#'
#' @keywords internal
.pdist <- function(dist) {
  return(switch(dist,
    plnorm = stats::plnorm,
    pgamma = stats::pgamma,
    pweibull = stats::pweibull,
    get(dist, envir = asNamespace("stats"))
  ))
}

.get_supported_dists <- function() {
  return(c("plnorm", "pgamma", "pweibull"))
}

.transpose_named_list2 <- function(lst) {
  n <- length(lst[[1]])
  result <- lapply(seq_len(n), function(i) {
    return(setNames(lapply(lst, `[`, i), names(lst)))
  })
  return(result)
}

#' Create a function to draw from the posterior predictive distribution for a
#' double censored and truncated delay distribution
#'
#' This function creates a function that draws from the posterior predictive
#' distribution for a latent model using [primarycensored::rpcens()] to handle
#' censoring and truncation. The returned function takes a `prep` argument from
#' `brms` and returns posterior predictions. This is used internally by
#' [brms::posterior_predict()] to generate predictions for latent models.
#'
#' @inheritParams epidist_family
#'
#' @return A function that takes a `prep` argument from brms and returns a
#' matrix of posterior predictions, with one row per posterior draw and one
#' column per observation. The `prep` object must have the following variables:
#' * `vreal1`: relative observation time
#' * `vreal2`: primary event window
#' * `vreal3`: secondary event window
#' * `vreal4`: delay upper bound
#' * `vreal5`: minimum delay (left truncation point; defaults to 0 if absent)
#'
#' @seealso [brms::posterior_predict()] for details on how this is used within
#' `brms`, [primarycensored::rpcens()] for details on the censoring approach
#' @autoglobal
#' @family gen
#' @export
epidist_gen_posterior_predict <- function(family) {
  dist_fn <- .get_brms_fn("posterior_predict", family)

  rdist <- function(n, i, prep, ...) {
    prep$ndraws <- n
    result <- do.call(dist_fn, list(i = i, prep = prep))
    return(result)
  }

  .predict <- function(i, prep, ...) {
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]
    delay_min <- if (is.null(prep$data$vreal5)) 0 else prep$data$vreal5[i]

    result <- as.matrix(primarycensored::rpcens(
      n = prep$ndraws,
      rdist = rdist,
      rprimary = stats::runif,
      pwindow = pwindow,
      swindow = swindow,
      L = delay_min,
      D = relative_obs_time,
      i = i,
      prep = prep
    ))
    return(result)
  }
  return(.predict)
}

#' Create a function to draw from the expected value of the posterior predictive
#' distribution for a model
#'
#' This function creates a function that calculates the expected value of the
#' posterior predictive distribution for a latent model. The returned function
#' takes a `prep` argument (from brms) and returns posterior expected values.
#' This is used internally by [brms::posterior_epred()] to calculate expected
#' values for latent models.
#'
#' @inheritParams epidist_family
#'
#' @return A function that takes a prep argument from brms and returns a matrix
#' of posterior expected values, with one row per posterior draw and one column
#' per observation.
#'
#' @seealso [brms::posterior_epred()] for details on how this is used within
#' `brms`.
#' @autoglobal
#' @family gen
#' @export
epidist_gen_posterior_epred <- function(family) {
  result <- .get_brms_fn("posterior_epred", family)
  return(result)
}
