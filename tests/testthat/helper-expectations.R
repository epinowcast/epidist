expect_convergence <- function(
  fit,
  per_dts = 0.05,
  treedepth = 10,
  rhat = 1.05
) {
  diagnostics <- epidist_diagnostics(fit)
  testthat::expect_lt(diagnostics$per_divergent_transitions, per_dts)
  testthat::expect_lt(diagnostics$max_treedepth, treedepth)
  testthat::expect_lt(diagnostics$max_rhat, rhat)
  return(invisible(TRUE))
}
