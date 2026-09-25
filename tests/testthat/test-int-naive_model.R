test_that("epidist.epidist_naive_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- epidist(
    data = prep_naive_obs,
    fn = brms::make_stancode
  )
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_naive_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  withr::local_seed(1)
  fit <- epidist(
    data = prep_naive_obs,
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2
  )
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_naive_model fits the gengamma family and predicts from it", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not_installed("flexsurv")
  set.seed(1)
  fit <- epidist(
    data = prep_naive_obs,
    family = gengamma(),
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2
  )
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
  draws <- add_summaries(delay_parameter_draws(fit), probs = 0.5)
  expect_true(all(
    c("mu", "sigma", "Q", "mean", "sd", "q50") %in% names(draws)
  ))
  expect_true(all(draws$mean > 0))
  # brms uses the family's own functions for a naive fit
  log_lik <- brms::log_lik(fit, draw_ids = 1:5)
  expect_identical(dim(log_lik), c(5L, nrow(fit$data)))
  expect_true(all(is.finite(log_lik)))
  expect_equal(
    as.numeric(log_lik[, 1]),
    fit$data$n[1] * flexsurv::dgengamma(
      fit$data$delay[1],
      mu = draws$mu[1:5], sigma = draws$sigma[1:5], Q = draws$Q[1:5],
      log = TRUE
    ),
    tolerance = 1e-6
  )
  epred <- brms::posterior_epred(fit, draw_ids = 1:5)
  expect_equal(as.numeric(epred[, 1]), draws$mean[1:5], tolerance = 1e-6)
  pred <- brms::posterior_predict(fit, draw_ids = 1:5)
  expect_identical(dim(pred), dim(log_lik))
  expect_true(all(pred > 0))
})
