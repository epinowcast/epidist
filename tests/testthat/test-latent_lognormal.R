test_that("posterior_predict_latent_lognormal outputs positive integers with length equal to draws", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit)
  i <- 1
  pred_i <- posterior_predict_latent_lognormal(i = i, prep)
  expect_equal(floor(pred_i), pred_i)
  expect_equal(length(pred_i), prep$ndraws)
  expect_gte(min(pred_i), 0)
})

test_that("posterior_predict_latent_lognormal errors for i out of bounds", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit)
  i_out_of_bounds <- length(prep$data$Y) + 1
  expect_error(posterior_predict_latent_lognormal(i = i_out_of_bounds, prep))
})

test_that("posterior_predict_latent_lognormal can generate predictions with no censoring", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  draws <- data.frame(obs_t = 1000, pwindow_upr = 0, swindow_upr = 0) |>
    tidybayes::add_predicted_draws(fit, ndraws = 100)
  expect_equal(draws$.draw, 1:100)
  pred <- draws$.prediction
  expect_gte(min(pred), 0)
  expect_true(all(abs(pred - round(pred)) > .Machine$double.eps^0.5))
})

test_that("posterior_predict_latent_lognormal predicts delays for which the data is in the 95% credible interval", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit)
  prep$ndraws <- 1000 # Down from the 4000 for time saving
  q <- purrr::map_vec(seq_along(prep$data$Y), function(i) {
    y <- posterior_predict_latent_lognormal(i, prep)
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

test_that("posterior_epred_latent_lognormal creates a array of non-negative numbers with the correct dimensions", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit)
  epred <- posterior_epred_latent_lognormal(prep)
  expect_setequal(class(epred), c("matrix", "array"))
  expect_equal(nrow(epred), prep$ndraws)
  expect_equal(ncol(epred), length(prep$data$Y))
  expect_gte(min(epred), 0)
})

test_that("log_lik_latent_lognormal produces a vector with length ndraws of finite non-NA numbers", { # nolint: line_length_linter.
  fit <- readRDS(
    system.file("extdata/fit.rds", package = "epidist")
  )
  prep <- brms::prepare_predictions(fit)
  i <- 1
  log_lik <- log_lik_latent_lognormal(i, prep)
  expect_equal(length(log_lik), prep$ndraws)
  expect_true(all(!is.na(log_lik)))
  expect_true(all(is.finite(log_lik)))
})
