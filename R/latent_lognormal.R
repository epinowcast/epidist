data <- as_latent_individual(sim_obs)
fit <- epidist(data)
prep <- brms::prepare_predictions(fit)

log_lik_latent_lognormal <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  y <- prep$data$Y[i]
}

posterior_predict_latent_lognormal <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  y <- prep$data$Y[i]
}

posterior_epred_latent_lognormal <- function(prep) {
  mu <- brms::get_dpar(prep, "mu")
  sigma <- brms::get_dpar(prep, "sigma")
}
