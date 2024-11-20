test_that("epidist_gen_posterior_predict returns a function that outputs positive integers with length equal to draws", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  prep <- brms::prepare_predictions(fit)
  i <- 1
  predict_fn <- epidist_gen_posterior_predict(lognormal())
  pred_i <- predict_fn(i = i, prep)
  expect_identical(floor(pred_i), pred_i)
  expect_length(pred_i, prep$ndraws)
  expect_gte(min(pred_i), 0)

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  predict_fn_gamma <- epidist_gen_posterior_predict(Gamma())
  pred_i_gamma <- predict_fn_gamma(i = i, prep_gamma)
  expect_identical(floor(pred_i_gamma), pred_i_gamma)
  expect_length(pred_i_gamma, prep_gamma$ndraws)
  expect_gte(min(pred_i_gamma), 0)
})

test_that("epidist_gen_posterior_predict returns a function that errors for i out of bounds", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  prep <- brms::prepare_predictions(fit)
  i_out_of_bounds <- length(prep$data$Y) + 1
  predict_fn <- epidist_gen_posterior_predict(lognormal())
  expect_warning(
    expect_error(
      predict_fn(i = i_out_of_bounds, prep)
    )
  )

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  i_out_of_bounds_gamma <- length(prep_gamma$data$Y) + 1
  predict_fn_gamma <- epidist_gen_posterior_predict(Gamma())
  expect_warning(
    expect_error(predict_fn_gamma(i = i_out_of_bounds_gamma, prep_gamma))
  )
})

test_that("epidist_gen_posterior_predict returns a function that can generate predictions with no censoring", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  predict_fn <- epidist_gen_posterior_predict(lognormal())
  draws <- data.frame(relative_obs_time = 1000, pwindow = 0, swindow = 0) |>
    tidybayes::add_predicted_draws(fit, ndraws = 100)
  expect_identical(draws$.draw, 1:100)
  pred <- draws$.prediction
  expect_gte(min(pred), 0)
  expect_true(all(abs(pred - round(pred)) > .Machine$double.eps^0.5))

  # Test gamma
  predict_fn_gamma <- epidist_gen_posterior_predict(Gamma())
  draws_gamma <- data.frame(
    relative_obs_time = 1000, pwindow = 0, swindow = 0
  ) |>
    tidybayes::add_predicted_draws(fit_gamma, ndraws = 100)
  expect_identical(draws_gamma$.draw, 1:100)
  pred_gamma <- draws_gamma$.prediction
  expect_gte(min(pred_gamma), 0)
  expect_true(
    all(abs(pred_gamma - round(pred_gamma)) > .Machine$double.eps^0.5)
  )
})

test_that("epidist_gen_posterior_predict returns a function that predicts delays in the 95% credible interval", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  prep <- brms::prepare_predictions(fit)
  prep$ndraws <- 1000 # Down from the 4000 for time saving
  predict_fn <- epidist_gen_posterior_predict(lognormal())
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

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  prep_gamma$ndraws <- 1000
  predict_fn_gamma <- epidist_gen_posterior_predict(Gamma())
  q_gamma <- purrr::map_vec(seq_along(prep_gamma$data$Y), function(i) {
    y <- predict_fn_gamma(i, prep_gamma)
    ecdf <- ecdf(y)
    q <- ecdf(prep_gamma$data$Y[i])
    return(q)
  })
  expect_lt(quantile(q_gamma, 0.1), 0.3)
  expect_gt(quantile(q_gamma, 0.9), 0.7)
  expect_lt(min(q_gamma), 0.1)
  expect_gt(max(q_gamma), 0.9)
  expect_lt(mean(q_gamma), 0.65)
  expect_gt(mean(q_gamma), 0.35)
})

test_that("epidist_gen_posterior_epred returns a function that creates arrays with correct dimensions", { # nolint: line_length_linter.
  skip_on_cran()
  # Test lognormal
  epred <- prep_obs |>
    tidybayes::add_epred_draws(fit)
  expect_equal(mean(epred$.epred), 5.97, tolerance = 0.1)
  expect_gte(min(epred$.epred), 0)

  # Test gamma
  prep_gamma <- brms::prepare_predictions(fit_gamma)
  epred_fn_gamma <- epidist_gen_posterior_epred(Gamma())
  epred_gamma <- epred_fn_gamma(prep_gamma)
  expect_setequal(class(epred_gamma), c("matrix", "array"))
  expect_identical(nrow(epred_gamma), prep_gamma$ndraws)
  expect_identical(ncol(epred_gamma), length(prep_gamma$data$Y))
  expect_gte(min(epred_gamma), 0)
})
