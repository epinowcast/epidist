#' Create a function to calculate the marginalised log likelihood for double
#' censored and truncated delay distributions
#'
#' This function creates a log likelihood function that calculates the marginal
#' likelihood for a single observation by integrating over the latent primary
#' and secondary event windows. The integration is performed numerically using
#' [primarycensored::dpcens()] which handles the double censoring and truncation
#' of the delay distribution.
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
#' @importFrom cli cli_warn
epidist_gen_log_lik <- function(family) {
  # Get internal brms log_lik function
  log_lik_brms <- .get_brms_fn("log_lik", family)

  pdist <- function(q, i, prep, ...) {
    # Check and warn if lower bound is set
    if (!is.null(prep$data$lb)) {
      cli_warn("Lower bound should not be set for model likelihood")
      prep$data$lb <- NULL
    }
    # Set upper bound for CDF calculation
    prep$data$ub <- rep(q, length(prep$data$Y))
    exp(log_lik_brms(i, prep))
  }

  .log_lik <- function(i, prep) {
    y <- prep$data$Y[i]
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]

    # Calculate density using primarycensored::dpcens()
    lpdf <- primarycensored::dpcens(
      x = y,
      pdist = pdist,
      i = i,
      prep = prep,
      pwindow = pwindow,
      swindow = swindow,
      D = relative_obs_time,
      dprimary = stats::dunif,
      log = TRUE
    )

    return(lpdf)
  }

  return(.log_lik)
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
  .get_brms_fn("posterior_epred", family)
}
