prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(prep_obs)
prep <- brms::prepare_predictions(fit)

test_that("posterior_predict_latent_lognormal outputs a positive single integer", { # nolint: line_length_linter.
  i <- 1
  pred_i <- posterior_predict_latent_lognormal(i = i, prep)
  expect_equal(floor(pred_i), pred_i)
  expect_equal(class(pred_i), "numeric")
  expect_equal(length(pred_i), 1)
  expect_gt(pred_i, 0)

  i_out_of_bounds <- length(prep$data$Y) + 1
  expect_error(posterior_predict_latent_lognormal(prep, i = i_out_of_bounds))
})

test_that("posterior_predict_latent_lognormal errors for i out of bounds", { # nolint: line_length_linter.
  i_out_of_bounds <- length(prep$data$Y) + 1
  expect_error(posterior_predict_latent_lognormal(prep, i = i_out_of_bounds))
})

test_that("posterior_predict_latent_lognormal predicts delays for which the data is in the 95% credible interval", { # nolint: line_length_linter.
  max_i <- length(prep$data$Y)
  df <- tidyr::crossing("i" = 1:max_i, "draw" = 1:100) |>
    dplyr::mutate(
      y = purrr::map_dbl(
        i, ~ posterior_predict_latent_lognormal(i = .x, prep = prep)
      )
    )
  quantiles <- purrr::map_vec(1:max_i, function(i) {
    ecdf <- ecdf(dplyr::filter(df, i == i)$y)
    q <- ecdf(prep$data$Y[i])
    return(q)
  })
  expect_lt(quantile(quantiles, 0.1), 0.3)
  expect_gt(quantile(quantiles, 0.9), 0.7)
  expect_lt(min(quantiles), 0.1)
  expect_gt(max(quantiles), 0.9)
})

test_that("posterior_epred_latent_lognormal creates a array of non-negative numbers with the correct dimensions", { # nolint: line_length_linter.
  epred <- posterior_epred_latent_lognormal(prep)
  expect_setequal(class(epred), c("matrix", "array"))
  expect_equal(nrow(epred), prep$ndraws)
  expect_equal(ncol(epred), length(prep$data$Y))
  expect_gte(min(epred), 0)
})
