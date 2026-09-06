test_that("epidist.epidist_naive_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- epidist(
    data = prep_naive_obs,
    fn = brms::make_stancode
  )
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  suppressMessages(expect_true(mod$check_syntax()))
})

test_that("epidist.epidist_naive_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  fit <- epidist(
    data = prep_naive_obs,
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2,
    backend = "cmdstanr"
  )
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

test_that("epidist.epidist_naive_model fits the gengamma family and predicts from it", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  skip_if_not_installed("flexsurv")
  set.seed(1)
  fit <- epidist(
    data = prep_naive_obs,
    family = gengamma(),
    seed = 1,
    silent = 2, refresh = 0,
    cores = 2,
    chains = 2,
    backend = "cmdstanr"
  )
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
  draws <- add_summaries(delay_parameter_draws(fit), probs = 0.5)
  expect_true(all(
    c("mu", "shape", "k", "mean", "sd", "q50") %in% names(draws)
  ))
  expect_true(all(draws$mean > 0))
  # brms uses the family's own functions for a naive fit
  log_lik <- brms::log_lik(fit, draw_ids = 1:5)
  expect_identical(dim(log_lik), c(5L, nrow(fit$data)))
  expect_true(all(is.finite(log_lik)))
  expect_equal(
    as.numeric(log_lik[, 1]),
    fit$data$n[1] * flexsurv::dgengamma.orig(
      fit$data$delay[1],
      shape = draws$shape[1:5], scale = draws$mu[1:5], k = draws$k[1:5],
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
