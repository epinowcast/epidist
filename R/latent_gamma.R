#' Draws from the posterior predictive distribution of the `latent_gamma` family
#'
#' See [brms::posterior_predict()].
#'
#' @param i The index of the observation to predict
#' @param prep The result of a call to [brms::posterior_predict()]
#' @param ... Additional arguments
#' @autoglobal
#' @importFrom stats rgamma
#' @keywords internal
posterior_predict_latent_gamma <- function(i, prep, ...) { # nolint: object_length_linter
  mu <- brms::get_dpar(prep, "mu", i = i)
  shape <- brms::get_dpar(prep, "shape", i = i)

  obs_t <- prep$data$vreal1[i]
  pwindow_width <- prep$data$vreal2[i]
  swindow_width <- prep$data$vreal3[i]

  .predict <- function(s) {
    d_censored <- obs_t + 1
    # while loop to impose the truncation
    while (d_censored > obs_t) {
      p_latent <- runif(1, 0, 1) * pwindow_width
      d_latent <- rgamma(1, shape = shape[s], scale = mu[s] / shape[s])
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

#' Draws from the expected value of the posterior predictive distribution of the
#' `latent_gamma` family
#'
#' See [brms::posterior_epred()].
#'
#' @param prep The result of a call to [`brms::prepare_predictions`]
#' @autoglobal
#' @keywords internal
posterior_epred_latent_gamma <- function(prep) { # nolint: object_length_linter
  mu <- brms::get_dpar(prep, "mu")
  mu
}

#' Calculate the pointwise log likelihood of the `latent_gamma` family
#'
#' See [brms::log_lik()].
#'
#' @param i The index of the observation to calculate the log likelihood of
#' @param prep The result of a call to [brms::prepare_predictions()]
#' @autoglobal
#' @importFrom stats dgamma pgamma
#' @keywords internal
log_lik_latent_gamma <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  shape <- brms::get_dpar(prep, "shape", i = i)
  y <- prep$data$Y[i]
  obs_t <- prep$data$vreal1[i]
  pwindow_width <- prep$data$vreal2[i]
  swindow_width <- prep$data$vreal3[i]

  swindow_raw <- runif(prep$ndraws)
  pwindow_raw <- runif(prep$ndraws)

  swindow <- swindow_raw * swindow_width

  # For no overlap calculate as usual, for overlap ensure pwindow < swindow
  if (i %in% prep$data$noverlap) {
    pwindow <- pwindow_raw * pwindow_width
  } else {
    pwindow <- pwindow_raw * swindow
  }

  d <- y - pwindow + swindow
  obs_time <- obs_t - pwindow
  lpdf <- dgamma(d, shape = shape, scale = mu / shape, log = TRUE)
  lcdf <- pgamma(obs_time, shape = shape, scale = mu / shape, log.p = TRUE)
  return(lpdf - lcdf)
}
