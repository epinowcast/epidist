expect_convergence <- function(
    fit,
    per_dts = 0.05,
    treedepth = 10,
    rhat = 1.05) {
  diag <- epidist_diagnostics(fit)
  testthat::expect_lt(diag$per_divergent_transitions, per_dts)
  testthat::expect_lt(diag$max_treedepth, treedepth)
  testthat::expect_lt(diag$max_rhat, rhat)
}
