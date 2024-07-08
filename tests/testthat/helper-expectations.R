expect_convergence <- function(fit, per_dts = 0.05, treedepth = 10,
                               rhat = 1.05) {
  div <- subset(brms::nuts_params(fit), Parameter == "divergent__")
  per_divergent_transitions <- mean(div$Value)
  max_treedepth <- brms::control_params(fit)$max_treedepth
  max_rhat <- max(brms::rhat(fit))
  testthat::expect_lt(per_divergent_transitions, per_dts)
  testthat::expect_lt(max_treedepth, treedepth)
  testthat::expect_lt(max_rhat, rhat)
}
