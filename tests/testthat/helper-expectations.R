get_sampler_params <- function(fit) {
  sp <- lapply(fit$fit@sim$samples, function(x) attr(x, "sampler_params"))
  do.call(rbind, sp)
}

expect_convergence <- function(fit, per_dts = 0.05, treedepth = 10,
                               rhat = 1.05) {
  np <- get_sampler_params(fit)
  per_divergent_transitions <- mean(np$divergent__)
  max_treedepth <- max(np$treedepth__)
  max_rhat <- max(brms::rhat(fit))
  testthat::expect_lt(per_divergent_transitions, per_dts)
  testthat::expect_lt(max_treedepth, treedepth)
  testthat::expect_lt(max_rhat, rhat)
}
