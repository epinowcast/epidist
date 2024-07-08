expect_convergence <- function(fit, per_dts = 0.05, treedepth = 10,
                               rhat = 1.05) {
  np <- brms::nuts_params(fit)
  indices <- np$Parameter == "divergent__"
  per_divergent_transitions <- mean(np[indices, ]$Value)
  max_treedepth <- brms::control_params(fit)$max_treedepth
  max_rhat <- max(brms::rhat(fit))
  testthat::expect_lt(per_divergent_transitions, per_dts)
  testthat::expect_lt(max_treedepth, treedepth)
  testthat::expect_lt(max_rhat, rhat)
}
