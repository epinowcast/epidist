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
#'
#' @family gen
#' @autoglobal
#' @importFrom purrr map_dbl
epidist_gen_log_lik <- function(family) {
  # Get internal brms log_lik function
  log_lik_brms <- .get_brms_fn("log_lik", family)

  # Get the name of the primary distribution
  primary_dist_name <- primarycensored::pcd_dist_name(tolower(family$family))

  # Check if family is supported with a analytical solution
  if (primary_dist_name %in% .get_supported_dists()) {
    .log_lik <- .analytical_gen_log_lik(primary_dist_name)
  } else {
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

    # make the prep object censored
    # -1 here is equivalent to right censored in brms
    # this means we get the cdf of the target distribution
    prep$data$cens <- rep(-1, prep$nobs)

    # Calculate density for each draw using primarycensored::dpcens()
    lpdf <- purrr::map_dbl(seq_len(prep$ndraws), function(draw) {
      # Define pdist function that filters to current draw
      pdist_draw <- function(q, i, prep, ...) {
        purrr::map_dbl(q, function(x) {
          prep$data$Y <- rep(x, length(prep$data$Y))
          prep$data$weights <- NULL
          prep$ndraws <- 1
          ll <- exp(log_lik_brms(i, prep)[draw])
          return(ll)
        })
      }

      primarycensored::dpcens(
        x = y,
        pdist = pdist_draw,
        i = i,
        prep = prep,
        pwindow = pwindow,
        swindow = swindow,
        D = relative_obs_time,
        dprimary = stats::dunif,
        log = TRUE
      )
    })
    lpdf <- brms:::log_lik_weight(lpdf, i = i, prep = prep) # nolint
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

    # Get distribution-specific parameters
    args <- .get_supported_dist_args(dist, prep, i)

    # Calculate density for each draw using primarycensored::dpcens()
    lpdf <- purrr::map_dbl(seq_len(prep$ndraws), function(draw) {
      do.call(
        primarycensored::dpcens,
        c(
          list(
            x = y,
            pdist = get(dist, envir = asNamespace("stats")),
            pwindow = pwindow,
            swindow = swindow,
            D = relative_obs_time,
            dprimary = stats::dunif,
            log = TRUE
          ),
          args[[draw]]
        )
      )
    })
    lpdf <- brms:::log_lik_weight(lpdf, i = i, prep = prep) # nolint
    return(lpdf)
  }

  return(.log_lik)
}

# Helper to get distribution-specific arguments
.get_supported_dist_args <- function(dist, prep, i) {
  args <- switch(dist,
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
  return(.transpose_named_list2(args))
}

.get_supported_dists <- function() {
  c("plnorm", "pgamma", "weibull")
}

.transpose_named_list2 <- function(lst) {
  n <- length(lst[[1]])
  lapply(seq_len(n), function(i) {
    setNames(lapply(lst, `[`, i), names(lst))
  })
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
    do.call(dist_fn, list(i = i, prep = prep))
  }

  .predict <- function(i, prep, ...) {
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]

    as.matrix(primarycensored::rpcens(
      n = prep$ndraws,
      rdist = rdist,
      rprimary = stats::runif,
      pwindow = prep$data$vreal2[i],
      swindow = prep$data$vreal3[i],
      D = prep$data$vreal1[i],
      i = i,
      prep = prep
    ))
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
  return(.get_brms_fn("posterior_epred", family))
}
