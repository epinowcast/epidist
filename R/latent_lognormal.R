#' Draws from the posterior predictive distribution
#'
#' See [brms::posterior_predict()].
#'
#' @param i The index of the observation to predict
#' @param prep The result of a call to [brms::posterior_predict()]
#' @param ... Additional arguments
#' @family postprocess
#' @autoglobal
#' @export
posterior_predict_latent_lognormal <- function(i, prep, ...) { # nolint: object_length_linter
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)

  obs_t <- prep$data$vreal1[i]
  pwindow_width <- prep$data$vreal2[i]
  swindow_width <- prep$data$vreal3[i]

  .predict <- function(s) {
    d_censored <- obs_t + 1
    # while loop to impose the truncation
    while (d_censored > obs_t) {
      p_latent <- runif(1, 0, 1) * pwindow_width
      d_latent <- rlnorm(1, meanlog = mu[s], sdlog = sigma[s])
      s_latent <- p_latent + d_latent
      p_censored <- floor_mult(p_latent, pwindow_width)
      s_censored <- floor_mult(s_latent, swindow_width)
      d_censored <- s_censored - p_censored
    }
    return(d_censored)
  }

  # Within brms this is a helper function called rblapply
  do.call(rbind, lapply(seq_len(prep$ndraws), .predict))
}

#' Draws from the expected value of the posterior predictive distribution
#'
#' See [brms::posterior_epred()].
#'
#' @param prep The result of a call to [`brms::prepare_predictions`]
#' @family postprocess
#' @autoglobal
#' @export
posterior_epred_latent_lognormal <- function(prep) { # nolint: object_length_linter
  mu <- brms::get_dpar(prep, "mu")
  sigma <- brms::get_dpar(prep, "sigma")
  exp(mu + sigma^2 / 2)
}

#' Calculate the pointwise log likelihood
#'
#' See [`brms::log_lik].
#'
#' @param i The index of the observation to calculate the log likelihood of
#' @param prep The result of a call to [`brms::prepare_predictions`]
#' @family postprocess
#' @autoglobal
#' @export
log_lik_latent_lognormal <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  y <- prep$data$Y[i]
  obs_t <- prep$data$vreal1[i]
  pwindow_width <- prep$data$vreal2[i]
  swindow_width <- prep$data$vreal3[i]

  # Generate values of the swindow_raw and pwindow_raw
  # Really these should be extracted from prep (no way to do that currently)
  swindow_raw <- runif(prep$ndraws)
  pwindow_raw <- runif(prep$ndraws)

  pwindow <- pwindow_raw * pwindow_width
  swindow <- swindow_raw * swindow_width

  # Still need to handle the "overlap" stuff in here, just leaving it for now
  # prep$data$noverlap
  # prep$data$wN
  # prep$data$woverlap

  d <- y - pwindow + swindow
  obs_time <- obs_t - pwindow
  lpdf <- dlnorm(d, meanlog = mu, sdlog = sigma, log = TRUE)
  lcdf <- plnorm(obs_time, meanlog = mu, sdlog = sigma, log.p = TRUE)
  return(lpdf - lcdf)
}
