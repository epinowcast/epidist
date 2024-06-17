expect_convergence <- function(fit, per_dts = 0.05, treedepth = 10,
                               rhat = 1.05) {

  max_treedepth <- rstan::get_num_max_treedepth(fit$fit)
  max_rhat <- max(brms::rhat(fit))
  
  #testthat::expect_lt(per_divergent_transitions, per_dts)
  testthat::expect_lt(max_treedepth, treedepth)
  testthat::expect_lt(max_rhat, rhat)
}
