# fmt: skip file
test_that(
  "predict_delay_parameters works with NULL newdata and the latent and marginal lognormal model", # nolint: line_length_linter.
  {
    skip_on_cran()

    # Helper function to test predictions
    test_predictions <- function(fit, expected_rows = nrow(prep_obs)) {
      set.seed(1)
      pred <- predict_delay_parameters(fit)
      expect_s3_class(pred, "lognormal_samples")
      expect_s3_class(pred, "data.frame")
      expect_named(pred, c("draw", "index", "mu", "sigma", "mean", "sd"))
      expect_true(all(pred$mean > 0))
      expect_true(all(pred$sd > 0))
      expect_length(unique(pred$index), expected_rows)
      expect_length(unique(pred$draw), summary(fit)$total_ndraws)
    }

    # Test latent and marginal models
    test_predictions(fit)
    test_predictions(fit_marginal, expected_rows = 144)
  }
)

test_that(
  "predict_delay_parameters works with the naive lognormal model",
  {
    skip_on_cran()

    # Test naive model predictions
    set.seed(1)
    pred_naive <- predict_delay_parameters(fit_naive)
    expect_s3_class(pred_naive, "lognormal_samples")
    expect_s3_class(pred_naive, "data.frame")
    expect_named(pred_naive, c("draw", "index", "mu", "sigma", "mean", "sd"))
    expect_true(all(pred_naive$mean > 0))
    expect_true(all(pred_naive$sd > 0))
    expect_length(unique(pred_naive$index), nrow(prep_naive_obs))
    expect_length(unique(pred_naive$draw), summary(fit_naive)$total_ndraws)
  }
)


test_that("predict_delay_parameters accepts newdata arguments and prediction by sex recovers underlying parameters", { # nolint: line_length_linter.
  skip_on_cran()

  # Helper function to test sex predictions
  test_sex_predictions <- function(fit, prep = prep_obs_sex) {
    set.seed(1)
    prep <- prep |>
      dplyr::mutate(.row_id = dplyr::row_number())
    pred_sex <- predict_delay_parameters(fit, prep)
    expect_s3_class(pred_sex, "lognormal_samples")
    expect_s3_class(pred_sex, "data.frame")
    expect_named(pred_sex, c("draw", "index", "mu", "sigma", "mean", "sd"))
    expect_true(all(pred_sex$mean > 0))
    expect_true(all(pred_sex$sd > 0))
    expect_length(unique(pred_sex$index), nrow(prep))
    expect_length(unique(pred_sex$draw), summary(fit)$total_ndraws)

    pred_sex_summary <- pred_sex |>
      dplyr::left_join(
        dplyr::select(prep, index = .row_id, sex),
        by = "index"
      ) |>
      dplyr::group_by(sex) |>
      dplyr::summarise(
        mu = mean(mu),
        sigma = mean(sigma)
      )

    # Correct predictions of M
    expect_equal(
      as.numeric(pred_sex_summary[1, c("mu", "sigma")]),
      c(meanlog_m, sdlog_m),
      tolerance = 0.1
    )

    # Correction predictions of F
    expect_equal(
      as.numeric(pred_sex_summary[2, c("mu", "sigma")]),
      c(meanlog_f, sdlog_f),
      tolerance = 0.1
    )
  }

  # Test latent and marginal models
  test_sex_predictions(fit_sex)
  test_sex_predictions(fit_marginal_sex, prep_marginal_obs_sex)
})

test_that("add_mean_sd.lognormal_samples works with simulated lognormal distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  df <- dplyr::tibble(
    mu = rnorm(n = 100, mean = 1.8, sd = 0.1),
    sigma = rnorm(n = 100, mean = 0.5, sd = 0.05)
  )
  class(df) <- c("lognormal_samples", class(df))
  x <- add_mean_sd(df)
  expect_named(x, c("mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_mean_sd.gamma_samples works with simulated gamma distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  df <- dplyr::tibble(
    shape = rnorm(n = 100, mean = 2, sd = 0.1),
    rate = rnorm(n = 100, mean = 3, sd = 0.2)
  ) |>
    dplyr::mutate(mu = shape / rate)
  class(df) <- c("gamma_samples", class(df))
  x <- add_mean_sd(df)
  expect_named(x, c("shape", "rate", "mu", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_mean_sd.weibull_samples works with simulated weibull distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  df <- dplyr::tibble(
    mu = rnorm(n = 100, mean = 3, sd = 0.2),
    shape = rnorm(n = 100, mean = 2, sd = 0.1)
  )
  class(df) <- c("weibull_samples", class(df))
  x <- add_mean_sd(df)
  expect_named(x, c("mu", "shape", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})
