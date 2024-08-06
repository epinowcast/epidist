data <- as_latent_individual(sim_obs)
fit <- epidist(data)
prep <- brms::prepare_predictions(fit)

stancode <- epidist(data, fn = brms::make_stancode)

# Won't be able to use expose in the package but can try with it for now
# Need some solution for how not to write code twice? Could expose and put in
# package precompiled somehow?
brms::expose_functions(x = fit, vectorize = TRUE)

# real latent_lognormal_lpdf(vector y, vector mu, vector sigma, vector pwindow,
#                            vector swindow, array[] real obs_t) {
#   int n = num_elements(y);
#   vector[n] d = y - pwindow + swindow;
#   vector[n] obs_time = to_vector(obs_t) - pwindow;
#   return lognormal_lpdf(d | mu, sigma) - lognormal_lcdf(obs_time | mu, sigma);
# }

i <- 1

log_lik_latent_lognormal <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  y <- prep$data$Y[i]
  vreal1 <- prep$data$vreal1[i]
  vreal2 <- prep$data$vreal2[i]
  vreal3 <- prep$data$vreal3[i]
  
  # Maybe the issue here is that I need to be integrating these out...
  # swindow_raw <- ...
  # pwindow_raw <- ...
  
  # pwindow <- ...
  # swindow <- ... 
  # obs_t <- ...
  latent_lognormal_lpdf(y, mu, sigma, pwindow, swindow, obs_t)
}

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

posterior_epred_latent_lognormal <- function(prep) {
  mu <- brms::get_dpar(prep, "mu")
  sigma <- brms::get_dpar(prep, "sigma")
}

