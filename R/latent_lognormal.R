posterior_predict_latent_lognormal <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  vreal1 <- prep$data$vreal1[i]
  vreal2 <- prep$data$vreal2[i]
  vreal3 <- prep$data$vreal3[i]
  wN <- prep$data$wN[i]
  woverlap <- prep$data$woverlap[i]
  noverlap <- prep$data$noverlap[i]
  
  swindow_raw <- runif(prep$ndraws, 0, 1)
  pwindow_raw <- runif(prep$ndraws, 0, 1)
  
  swindow <- vreal3 * swindow_raw
  # pwindow <- ...
  # obs_t <- ...
  
  y <- prep$data$Y[i]
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
