# fmt: skip file
test_that("epidist_gen_posterior_predict returns a function that outputs positive integers with length equal to draws", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test predictions
  test_predictions <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    i <- 1
    predict_fn <- epidist_gen_posterior_predict(family)
    pred_i <- predict_fn(i = i, prep)
    expect_identical(floor(pred_i), pred_i)
    expect_length(pred_i, prep$ndraws)
    expect_gte(min(pred_i), 0)
  }

  # Test lognormal - latent and marginal
  test_predictions(fit, lognormal())
  test_predictions(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_predictions(fit_gamma, Gamma())
  test_predictions(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that errors for i out of bounds", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test out of bounds errors
  test_out_of_bounds <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    i_out_of_bounds <- length(prep$data$Y) + 1
    predict_fn <- epidist_gen_posterior_predict(family)
    suppressMessages(expect_warning(
      expect_error(
        predict_fn(i = i_out_of_bounds, prep)
      )
    ))
  }

  # Test lognormal - latent and marginal
  test_out_of_bounds(fit, lognormal())
  test_out_of_bounds(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_out_of_bounds(fit_gamma, Gamma())
  test_out_of_bounds(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that can generate predictions with no censoring", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test uncensored predictions
  test_uncensored <- function(fit, family) {
    predict_fn <- epidist_gen_posterior_predict(family)
    draws <- data.frame(
      relative_obs_time = Inf, pwindow = 0, swindow = 0, delay_upr = NA
    ) |>
      tidybayes::add_predicted_draws(fit, ndraws = 100)
    expect_identical(draws$.draw, 1:100)
    pred <- draws$.prediction
    expect_gte(min(pred), 0)
    expect_true(all(abs(pred - round(pred)) > .Machine$double.eps^0.5))
  }

  # Test lognormal - latent and marginal
  test_uncensored(fit, lognormal())
  test_uncensored(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_uncensored(fit_gamma, Gamma())
  test_uncensored(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that predicts delays in the 95% credible interval", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test credible intervals
  test_credible_intervals <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    prep$ndraws <- 1000 # Down from the 4000 for time saving
    predict_fn <- epidist_gen_posterior_predict(family)
    q <- purrr::map_vec(seq_along(prep$data$Y), function(i) {
      y <- predict_fn(i, prep)
      ecdf <- ecdf(y)
      q <- ecdf(prep$data$Y[i])
      return(q)
    })
    expect_lt(quantile(q, 0.1), 0.3)
    expect_gt(quantile(q, 0.9), 0.7)
    expect_lt(min(q), 0.1)
    expect_gt(max(q), 0.9)
    expect_lt(mean(q), 0.65)
    expect_gt(mean(q), 0.35)
  }

  # Test lognormal - latent and marginal
  test_credible_intervals(fit, lognormal())
  test_credible_intervals(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_credible_intervals(fit_gamma, Gamma())
  test_credible_intervals(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_epred returns a function that creates arrays with correct dimensions", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test epred
  test_epred <- function(fit, expected_mean) {
    epred <- prep_obs |>
      mutate(delay_upr = NA) |>
      tidybayes::add_epred_draws(fit)
    expect_equal(mean(epred$.epred), expected_mean, tolerance = 0.1)
    expect_gte(min(epred$.epred), 0)
  }

  # Test lognormal - latent and marginal
  test_epred(fit, 5.97)
  test_epred(fit_marginal, 5.97)

  # Test gamma - latent and marginal
  test_epred(fit_gamma, 6.56)
  test_epred(fit_marginal_gamma, 6.56)
})

test_that("epidist_gen_log_lik returns a function that produces valid log likelihoods", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  prep <- brms::prepare_predictions(fit)
  prep$ndraws <- 10
  i <- 1
  log_lik_fn <- epidist_gen_log_lik(lognormal())
  log_lik <- log_lik_fn(i = i, prep)
  expect_length(log_lik, prep$ndraws)
  expect_false(anyNA(log_lik))
  expect_true(all(is.finite(log_lik)))

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  prep$ndraws <- 10
  log_lik_fn_gamma <- epidist_gen_log_lik(Gamma())
  log_lik_gamma <- log_lik_fn_gamma(i = i, prep_gamma)
  expect_length(log_lik_gamma, prep_gamma$ndraws)
  expect_false(anyNA(log_lik_gamma))
  expect_true(all(is.finite(log_lik_gamma)))
})
