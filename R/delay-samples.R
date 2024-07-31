#' @export
delay_samples <- function(fit) {
  # Warning: only works at the moment with lognormal!
  pp <- brms::prepare_predictions(fit)
  lp_mu <- brms::get_dpar(pp, dpar = "mu", inv_link = TRUE)
  lp_sigma <- brms::get_dpar(pp, dpar = "sigma", inv_link = TRUE)
  # Assumes lp_mu and lp_sigma have same dimensions
  df <- expand.grid("index" = 1:nrow(lp_mu), "draw" = 1:ncol(lp_mu))
  df[["mu"]] <- as.vector(lp_mu)
  df[["sigma"]] <- as.vector(lp_sigma)
  class(df) <- c(class(df), "lognormal_samples")
  dt <- as.data.table(df)
  dt <- add_mean_sd(dt)
}

# data <- as_latent_individual(sim_obs)
# fit <- epidist(data)
