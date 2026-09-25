# fmt: skip file
test_that("epidist.epidist_marginal_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_marginal_obs,
    fn = brms::make_stancode
  ))
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_marginal_model fits and the MCMC converges in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  expect_s3_class(fit_marginal, "brmsfit")
  expect_s3_class(fit_marginal, "epidist_fit")
  expect_convergence(fit_marginal)
})

test_that("epidist.epidist_marginal_model recovers the simulation settings for the delay distribution in the default case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  withr::local_seed(1)
  pred <- delay_parameter_draws(fit_marginal)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_marginal_model fits and the MCMC converges in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  withr::local_seed(1)
  expect_s3_class(fit_marginal_gamma, "brmsfit")
  expect_s3_class(fit_marginal_gamma, "epidist_fit")
  expect_convergence(fit_marginal_gamma)
})

test_that("epidist.epidist_marginal_model recovers the simulation settings for the delay distribution in the gamma delay case", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  withr::local_seed(1)
  draws_gamma <- posterior::as_draws_df(fit_marginal_gamma$fit)
  draws_gamma_mu <- exp(draws_gamma$Intercept)
  draws_gamma_shape <- exp(draws_gamma$Intercept_shape)
  draws_gamma_mu_ecdf <- ecdf(draws_gamma_mu)
  draws_gamma_shape_ecdf <- ecdf(draws_gamma_shape)
  quantile_mu <- draws_gamma_mu_ecdf(mu)
  quantile_shape <- draws_gamma_shape_ecdf(shape)
  expect_gte(quantile_mu, 0.025)
  expect_lte(quantile_mu, 0.975)
  expect_gte(quantile_shape, 0.025)
  expect_lte(quantile_shape, 0.975)
})

test_that("epidist.epidist_marginal_model fits and recovers a sex effect", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  withr::local_seed(1)
  expect_s3_class(fit_marginal_sex, "brmsfit")
  expect_s3_class(fit_marginal_sex, "epidist_fit")
  expect_convergence(fit_marginal_sex)

  draws <- posterior::as_draws_df(fit_marginal_sex$fit)
  expect_equal(mean(draws$b_Intercept), meanlog_m, tolerance = 0.3)
  expect_equal(
    mean(draws$b_Intercept + draws$b_sex), meanlog_f,
    tolerance = 0.3
  )
  expect_equal(mean(exp(draws$b_sigma_Intercept)), sdlog_m, tolerance = 0.3)
  expect_equal(
    mean(exp(draws$b_sigma_Intercept + draws$b_sigma_sex)),
    sdlog_f,
    tolerance = 0.3
  )
})

test_that("epidist.epidist_marginal_model with a nonparametric delay recovers the mean of each stratum", { # nolint: line_length_linter.
  # The strata differ in the shape of the delay as well as its location, so
  # the linear part of the hazard spline varies by stratum as well as `mu`.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  set.seed(12)
  meanlog <- c(a = 1.2, b = 1.8)
  sdlog <- c(a = 0.5, b = 0.4)
  cases <- lapply(names(meanlog), function(group) {
    stratum <- simulate_gillespie() |>
      simulate_secondary(
        dist = rlnorm, meanlog = meanlog[[group]], sdlog = sdlog[[group]]
      ) |>
      dplyr::mutate(
        ptime_lwr = floor(.data$ptime), ptime_upr = .data$ptime_lwr + 1,
        stime_lwr = floor(.data$stime), stime_upr = .data$stime_lwr + 1,
        obs_time = 100
      ) |>
      dplyr::filter(.data$stime_upr <= .data$obs_time) |>
      dplyr::slice_sample(n = 300)
    stratum$group <- group
    return(stratum)
  })
  cases <- dplyr::bind_rows(cases)
  data <- suppressMessages(as_epidist_marginal_model(as_epidist_linelist_data(
    cases$ptime_lwr, cases$ptime_upr, cases$stime_lwr, cases$stime_upr,
    cases$obs_time,
    group = cases$group
  )))
  # Delays long past the observation time are set to an infinite
  # observation time, which brms warns about.
  fit <- suppressWarnings(suppressMessages(epidist(
    data,
    formula = bf(mu ~ 1 + group, h1b ~ 1 + group),
    family = nonparametric(),
    seed = 1, chains = 2, cores = 2, refresh = 0, iter = 1000, silent = 2
  )))
  expect_convergence(fit)
  # The post-processing brms does from the prep object works per
  # observation, as for the parametric families.
  log_lik <- brms::log_lik(fit, draw_ids = 1:5)
  expect_identical(dim(log_lik), c(5L, nrow(fit$data)))
  expect_true(all(is.finite(log_lik)))
  expect_s3_class(suppressWarnings(loo::loo(fit)), "loo")
  pred <- brms::posterior_predict(fit, draw_ids = 1:5)
  expect_identical(dim(pred), c(5L, nrow(fit$data)))
  epred <- brms::posterior_epred(fit, draw_ids = 1:5)
  expect_identical(dim(epred), c(5L, nrow(fit$data)))
  expect_true(all(epred > 0))
  summaries <- delay_summary_draws(fit)
  # The daily delay of a uniform primary event has the mean of the
  # continuous delay, which is what the bins recover.
  truth <- exp(meanlog + sdlog^2 / 2)
  for (group in names(truth)) {
    expect_equal(
      mean(summaries$mean[summaries$group == group]), truth[[group]],
      tolerance = 0.1
    )
  }
})

test_that("epidist.epidist_marginal_model fits the gengamma family and recovers a gamma delay", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not_installed("flexsurv")
  set.seed(1)
  fit <- suppressMessages(epidist(
    data = prep_marginal_obs_gamma,
    family = gengamma(),
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000
  ))
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
  # The data are gamma with shape 2 and scale 3, the shape = 1 special case,
  # so the delay mean and standard deviation are recovered even though the
  # two shape parameters trade off against each other
  draws <- add_summaries(delay_parameter_draws(fit))
  expect_equal(mean(draws$mean), shape / rate, tolerance = 0.1)
  expect_equal(mean(draws$sd), sqrt(shape) / rate, tolerance = 0.15)
  log_lik <- brms::log_lik(fit, draw_ids = 1:5)
  expect_true(all(is.finite(log_lik)))
  pred <- brms::posterior_predict(fit, draw_ids = 1:5)
  expect_identical(dim(pred), c(5L, nrow(fit$data)))
  expect_true(all(pred >= 0))
})
