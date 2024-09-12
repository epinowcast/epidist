test_that("predict_delay_parameters works with NULL newdata and the latent lognormal model", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs <- as_latent_individual(sim_obs)
  fit <- epidist(
    data = prep_obs,
    seed = 1,
    silent = 2,
    output_dir = fs::dir_create(tempfile())
  )
  pred <- predict_delay_parameters(fit)
  expect_s3_class(pred, "data.table")
  expect_named(pred, c("draw", "index", "mu", "sigma", "mean", "sd"))
  expect_true(all(pred$mean > 0))
  expect_true(all(pred$sd > 0))
  expect_equal(length(unique(pred$index)), nrow(prep_obs))
  expect_equal(length(unique(pred$draw)), summary(fit)$total_ndraws)
})

test_that("predict_delay_parameters accepts newdata arguments and prediction by sex recovers underlying parameters", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  prep_obs_sex <- as_latent_individual(sim_obs_sex)
  fit_sex <- epidist(
    data = prep_obs_sex,
    formula = brms::bf(mu ~ 1 + sex, sigma ~ 1 + sex),
    seed = 1,
    silent = 2
  )
  pred_sex <- predict_delay_parameters(fit_sex, prep_obs_sex)
  expect_s3_class(pred_sex, "data.table")
  expect_named(pred_sex, c("draw", "index", "mu", "sigma", "mean", "sd"))
  expect_true(all(pred_sex$mean > 0))
  expect_true(all(pred_sex$sd > 0))
  expect_equal(length(unique(pred_sex$index)), nrow(prep_obs_sex))
  expect_equal(length(unique(pred_sex$draw)), summary(fit_sex)$total_ndraws)

  pred_sex_summary <- pred_sex |>
    dplyr::left_join(
      dplyr::select(data.frame(prep_obs_sex), index = row_id, sex),
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
})

test_that("add_mean_sd.lognormal_samples works with simulated lognormal distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    mu = rnorm(n = 100, mean = 1.8, sd = 0.1),
    sigma = rnorm(n = 100, mean = 0.5, sd = 0.05)
  )
  class(dt) <- c(class(dt), "lognormal_samples")
  x <- add_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("mu", "sigma", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})

test_that("add_mean_sd.gamma_samples works with simulated gamma distribution parameter data", { # nolint: line_length_linter.
  set.seed(1)
  dt <- data.table(
    shape = rnorm(n = 100, mean = 2, sd = 0.1),
    rate = rnorm(n = 100, mean = 3, sd = 0.2)
  )
  dt[, mu := shape / rate]
  class(dt) <- c(class(dt), "gamma_samples")
  x <- add_mean_sd(dt)
  expect_s3_class(x, "data.table")
  expect_named(x, c("shape", "rate", "mu", "mean", "sd"))
  expect_true(all(x$mean > 0))
  expect_true(all(x$sd > 0))
})
