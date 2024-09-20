test_that("posterior_predict_latent_gamma outputs positive integers with length equal to draws", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit_gamma)
  i <- 1
  pred_i <- posterior_predict_latent_gamma(i = i, prep)
  expect_equal(floor(pred_i), pred_i)
  expect_equal(length(pred_i), prep$ndraws)
  expect_gte(min(pred_i), 0)
})

test_that("posterior_predict_latent_gamma errors for i out of bounds", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit_gamma)
  i_out_of_bounds <- length(prep$data$Y) + 1
  expect_error(posterior_predict_latent_gamma(i = i_out_of_bounds, prep))
})

test_that("posterior_predict_latent_gamma can generate predictions with no censoring", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  draws <- data.frame(relative_obs_time = 1000, pwindow = 0, swindow = 0) |>
    tidybayes::add_predicted_draws(fit_gamma, ndraws = 100)
  expect_equal(draws$.draw, 1:100)
  pred <- draws$.prediction
  expect_gte(min(pred), 0)
  expect_true(all(abs(pred - round(pred)) > .Machine$double.eps^0.5))
})

test_that("posterior_predict_latent_gamma predicts delays for which the data is in the 95% credible interval", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit_gamma)
  prep$ndraws <- 1000 # Down from the 4000 for time saving
  q <- purrr::map_vec(seq_along(prep$data$Y), function(i) {
    y <- posterior_predict_latent_gamma(i, prep)
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
})

test_that("posterior_epred_latent_gamma creates a array of non-negative numbers with the correct dimensions", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit_gamma)
  epred <- posterior_epred_latent_gamma(prep)
  expect_setequal(class(epred), c("matrix", "array"))
  expect_equal(nrow(epred), prep$ndraws)
  expect_equal(ncol(epred), length(prep$data$Y))
  expect_gte(min(epred), 0)
})

test_that("log_lik_latent_gamma produces a vector with length ndraws of finite non-NA numbers", { # nolint: line_length_linter.
  skip_on_cran()
  fit_gamma <- readRDS(
    system.file("extdata/fit_gamma.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit_gamma)
  i <- 1
  log_lik <- log_lik_latent_gamma(i, prep)
  expect_equal(length(log_lik), prep$ndraws)
  expect_true(all(!is.na(log_lik)))
  expect_true(all(is.finite(log_lik)))
})
