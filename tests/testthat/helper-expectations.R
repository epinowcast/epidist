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

# A simulation and recovery check that the central credible interval of
# `draws` brackets `truth`, allowing `margin_sd` posterior standard
# deviations of slack at each edge.
#
# The slack is not a concession to Monte Carlo error. The interval narrows
# with the size of the simulated studies while the bias of what they report
# does not, so the check tightens as the simulation grows. The grid recovery
# fit reads summaries of integer rounded, right truncated delays from about
# 19,000 cases, which leaves the posterior standard deviation of `sigma` at
# 0.0035 and its mean 0.0065 above a true 0.5. Its 2.5% quantile then sat
# 2.4e-5 above the truth, so the comparison was settled by the last digits
# of the platform's arithmetic and failed on macOS alone. See #733.
#
# One standard deviation of slack widens a 95% interval to roughly a 99.8%
# one, so a calibrated posterior misses about once in 500 checks rather than
# once in 20. It still fails once the recovered parameter is more than about
# three posterior standard deviations from the truth, which is 2% of `sigma`
# here, and so stays tighter than the tolerances on the posterior means
# beside it.
expect_recovers <- function(
  draws,
  truth,
  param = deparse(substitute(draws)),
  prob = 0.95,
  margin_sd = 1
) {
  tail_prob <- (1 - prob) / 2
  margin <- margin_sd * stats::sd(draws)
  edges <- stats::quantile(draws, c(tail_prob, 1 - tail_prob), names = FALSE)
  testthat::expect_lt(
    edges[[1]] - margin, truth,
    label = paste(param, "lower interval edge")
  )
  testthat::expect_gt(
    edges[[2]] + margin, truth,
    label = paste(param, "upper interval edge")
  )
  return(invisible(TRUE))
}
