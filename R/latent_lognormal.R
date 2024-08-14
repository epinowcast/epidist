#' Draws from the posterior predictive distribution
#'
#' See [`brms::posterior_predict`].
#'
#' @param i The index of the observation to predict
#' @param prep The result of a call to [`brms::posterior_predict`]
#' @family postprocess
#' @autoglobal
#' @export
posterior_predict_latent_lognormal <- function(i, prep) { # nolint: object_length_linter
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)

  obs_t <- prep$data$vreal1[i]
  pwindow_width <- prep$data$vreal2[i]
  swindow_width <- prep$data$vreal3[i]

  # while loop to impose the truncation
  d_censored <- obs_t + 1
  while (d_censored > obs_t) {
    p_latent <- runif(1, 0, 1) * pwindow_width
    d_latent <- rlnorm(1, meanlog = mu, sdlog = sigma)
    s_latent <- p_latent + d_latent
    p_censored <- floor_mult(p_latent, pwindow_width)
    s_censored <- floor_mult(s_latent, swindow_width)
    d_censored <- s_censored - p_censored
  }

  return(d_censored)
}

#' Draws from the expected value of the posterior predictive distribution
#'
#' See [`brms::posterior_epred`].
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
