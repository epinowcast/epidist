#' Draws from the posterior predictive distribution
#' 
#' See [`brms::posterior_predict`].
#'
#' @param i The index of the observation to predict
#' @param prep The result of a call to [`brms::posterior_predict`]
#' @family postprocess
#' @autoglobal
#' @export
posterior_predict_latent_lognormal <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)

  wN <- prep$data$wN[i]
  woverlap <- prep$data$woverlap[i]
  noverlap <- prep$data$noverlap[i]
  
  # This assumes a certain prior on swindow_raw and pwindow_raw
  # It would be better to extract these from prep or the model in some way?
  swindow_raw <- runif(1, 0, 1)
  pwindow_raw <- runif(1, 0, 1)
  
  vreal3 <- prep$data$vreal3[i]
  swindow <- vreal3 * swindow_raw
  
  vreal2 <- prep$data$vreal2[i]
  pwindow <- vreal2[noverlap] * pwindow_raw[noverlap]
  if (wN) {
    pwindow[woverlap] <- swindow[woverlap] * pwindow_raw[woverlap]
  }
  
  vreal1 <- prep$data$vreal1[i]
  obs_t <- vreal1
  obs_time <- obs_t - pwindow
  
  d <- EnvStats::rlnormTrunc(
    1,
    meanlog = mu,
    sdlog = sigma,
    min = 0,
    max = obs_time
  )
  
  y <- d + pwindow - swindow
  
  # Shouldn't this be censored to e.g. integer?
  return(y)
}

#' Draws from the expected value of the posterior predictive distribution
#' 
#' See [`brms::posterior_epred`].
#'
#' @param prep The result of a call to [`brms::prepare_predictions`]
#' @family postprocess
#' @autoglobal
#' @export
posterior_epred_latent_lognormal <- function(prep) {
  mu <- brms::get_dpar(prep, "mu")
  sigma <- brms::get_dpar(prep, "sigma")
  exp(mu + sigma^2 / 2)
}

#' The latent lognormal LPDF function
#' 
#' To be tested against an exposed Stan version. I think that this would be
#' used in the forthcoming `log_lik_latent_lognormal` function.
#' 
#' @export
latent_lognormal_lpdf <- function(y, mu, sigma, pwindow, swindow, obs_t) {
  n <- length(y)
  d <- y - pwindow + swindow
  obs_time <- obs_t - pwindow
  lpdf <- dlnorm(d, meanlog = mu, sdlog = sigma, log = TRUE)
  lcdf <- plnorm(obs_time, meanlog = mu, sdlog = sigma, log.p = TRUE)
  return(lpdf - lcdf)
}
