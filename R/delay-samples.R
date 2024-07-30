delay_samples <- function(fit) {
  dpars <- fit$family$dpars
  lp_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = dpars[1])
  lp_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = dpars[2])
  
  # lp_mu <- brms::posterior_linpred(fit, transform = TRUE, dpar = "mu") |>
  #   as.table() |>
  #   as.data.table()
  # names(lp_mu) <- c("draw", "index", "mu")
  # lp_sigma <- brms::posterior_linpred(fit, transform = TRUE, dpar = "sigma") |>
  #   as.table() |>
  #   as.data.table(value.name = "sigma")
  # names(lp_sigma) <- c("draw", "index", "sigma")
  # lp <- dplyr::left_join(lp_mu, lp_sigma)
  # class(lp) <- c(class(lp), "lognormal_samples")
  # x <- add_mean_sd(lp)
}
