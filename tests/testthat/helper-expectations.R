expect_convergence <- function(fit, per_dts = 0.05, treedepth = 10,
                               rhat = 1.05) {
  np <- brms::nuts_params(fit)
  divergent_indices <- np$Parameter == "divergent__"
  per_divergent_transitions <- mean(np[divergent_indices, ]$Value)
  treedepth_indices <- np$Parameter == "treedepth__"
  max_treedepth <- max(np[treedepth_indices, ]$Value)
  max_rhat <- max(brms::rhat(fit))
  testthat::expect_lt(per_divergent_transitions, per_dts)
  testthat::expect_lt(max_treedepth, treedepth)
  testthat::expect_lt(max_rhat, rhat)
}
