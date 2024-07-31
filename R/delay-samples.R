#' Extract samples of the delay distribution parameters
#'
#' @param fit A model fit with `epidist::epidist`
#' @family postprocess
#' @export
delay_samples <- function(fit) {
  pp <- brms::prepare_predictions(fit)
  # Every brms model has the parameter mu
  lp_mu <- brms::get_dpar(pp, dpar = "mu", inv_link = TRUE)
  df <- expand.grid("index" = 1:nrow(lp_mu), "draw" = 1:ncol(lp_mu))
  df[["mu"]] <- as.vector(lp_mu)
  for (dpar in setdiff(names(pp$dpars), "mu")) {
    lp_dpar <- brms::get_dpar(pp, dpar = dpar, inv_link = TRUE)
    df[[dpar]] <- as.vector(lp_dpar)
  }
  class(df) <- c(
    class(df), paste0(sub(".*_", "", fit$family$name), "_samples")
  )
  dt <- as.data.table(df)
  dt <- add_mean_sd(dt)
  return(dt)
}
