# fmt: skip file
test_that("epidist.epidist_meta_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a gamma delay", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = Gamma(link = "log"),
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a weibull delay", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = "weibull",
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with summary estimates only", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_s3_class(fit_meta_estimates, "brmsfit")
  expect_s3_class(fit_meta_estimates, "epidist_fit")
  expect_convergence(fit_meta_estimates)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from biased summary estimates", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_estimates)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.15)
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_s3_class(fit_meta_mixed, "brmsfit")
  expect_s3_class(fit_meta_mixed, "epidist_fit")
  expect_convergence(fit_meta_mixed)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_mixed)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_meta_model log_lik and posterior_predict have the expected shapes", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  log_lik <- brms::log_lik(fit_meta_estimates)
  expect_identical(ncol(log_lik), nrow(prep_meta_biased))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_estimates)
  expect_identical(ncol(pred), nrow(prep_meta_biased))
  expect_true(all(is.finite(pred)))
})

test_that("epidist.epidist_meta_model predicts individual level rows on the delay scale", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  prep <- brms::prepare_predictions(fit_meta_mixed)
  individual <- which(prep$data$vint1 == 1L)
  expect_gt(length(individual), 0L)
  expect_gt(sum(prep$data$vint1 != 1L), 0L)

  log_lik <- brms::log_lik(fit_meta_mixed)
  expect_identical(ncol(log_lik), length(prep$data$vint1))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_mixed)
  expect_identical(ncol(pred), length(prep$data$vint1))
  expect_true(all(is.finite(pred)))
  # Individual level rows are handed to the marginal model generators, so
  # they predict a censored delay rather than a reported summary.
  delays <- pred[, individual, drop = FALSE]
  expect_identical(floor(delays), delays)
  expect_gte(min(delays), 0)
})

test_that("the R and Stan meta model log likelihoods agree for every observation type", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = lockstep_estimates)
  )
  family <- epidist_family(meta, family = lognormal())
  formula <- epidist_formula(meta, family, formula = bf(mu ~ 1))
  stanvars <- epidist_stancode(meta, family = family, formula = formula)
  standata <- suppressMessages(epidist(meta, fn = brms::make_standata))
  slots <- c(paste0("vint", 1:8), paste0("vreal", 1:8))
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int N;\n  array[N] int Y;\n",
    paste0("  array[N] int ", slots[1:8], ";\n", collapse = ""),
    paste0("  array[N] real ", slots[9:16], ";\n", collapse = ""),
    "  int<lower=0> N_meta_group;\n",
    "  vector[N_meta_group] meta_group_value;\n",
    "  array[N_meta_group] int meta_group_count;\n",
    "  array[N_meta_group] int meta_group_type;\n",
    "  vector[N_meta_group] meta_group_p;\n",
    "  int<lower=0> N_meta_chol;\n",
    "  vector[N_meta_chol] meta_group_chol;\n",
    "  real mu;\n  real sigma;\n}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  vector[N] log_lik;\n  for (n in 1:N) {\n",
    "    log_lik[n] = meta_lognormal_lpmf(Y[n] | mu, sigma, ",
    paste0(slots, "[n]", collapse = ", "),
    ", meta_group_value, meta_group_count, meta_group_type, meta_group_p",
    ", meta_group_chol, primary_params);\n  }\n}\n"
  )))
  stan_data <- c(
    list(N = length(standata$Y), Y = as.integer(standata$Y)),
    lapply(standata[slots[1:8]], as.integer),
    lapply(standata[slots[9:16]], as.numeric),
    list(
      N_meta_group = standata$N_meta_group,
      meta_group_value = as.array(standata$meta_group_value),
      meta_group_count = as.array(standata$meta_group_count),
      meta_group_type = as.array(standata$meta_group_type),
      meta_group_p = as.array(standata$meta_group_p),
      N_meta_chol = standata$N_meta_chol,
      meta_group_chol = as.array(standata$meta_group_chol),
      mu = 1.7, sigma = 0.55
    )
  )
  fit <- mod$sample(
    data = stan_data, fixed_param = TRUE, chains = 1, iter_sampling = 1,
    iter_warmup = 0, refresh = 0, show_messages = FALSE
  )
  stan_log_lik <- as.numeric(posterior::as_draws_matrix(fit$draws("log_lik")))
  prep <- list(data = stan_data, ndraws = 1)
  args <- list(meanlog = stan_data$mu, sdlog = stan_data$sigma)
  r_log_lik <- vapply(
    seq_along(stan_log_lik),
    function(i) {
      return(.meta_row_log_lik(.meta_row_slots(i, prep), "plnorm", args))
    },
    numeric(1)
  )
  # Every observation type, censoring adjustment and truncation design must
  # be exercised, so that no branch is compared vacuously.
  expect_setequal(unique(standata$vint1), 2:7)
  expect_setequal(unique(standata$vint4), 0:4)
  expect_setequal(unique(standata$vint5), 0:1)
  expect_true(any(standata$vreal5 > 0))
  expect_true(any(standata$vreal6 > 0))
  expect_equal(stan_log_lik, r_log_lik, tolerance = 1e-6)
})

test_that("the Stan naive grid stays finite on a grid that runs into the tail", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  # Over a wide grid the primary censored distribution function saturates at
  # one, so its log stops increasing and differencing it returns NaN. The
  # default `max_delay` puts a truncation adjusted naive study in this region,
  # so the numbers Stan returns are checked here rather than the code it was
  # built from.
  meanlog <- 1
  sdlog <- 0.4
  cutoff <- c(30, 60, 100, 200)
  accrual <- c(0L, 0L, 0L, 1L)
  n_case <- length(cutoff)
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(20, 12), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 0, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  # The documented default runs the grid far enough into the tail to fail.
  expect_gte(estimates$max_delay[1], max(cutoff))
  stanvars <- epidist_stancode(meta)
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int<lower=1> N;\n  array[N] real cutoff;\n",
    "  array[N] int accrual;\n  real mu;\n  real sigma;\n}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  array[N] vector[4] moments;\n",
    "  array[N] real mass_min;\n  array[N] real mass_total;\n",
    "  for (n in 1:N) {\n",
    "    vector[to_int(floor(cutoff[n]))] mass = meta_lognormal_grid_pmf(\n",
    "      {mu, sigma}, 0, cutoff[n], 1, 1, 1, primary_params, accrual[n], 0\n",
    "    );\n",
    "    mass_min[n] = min(mass);\n    mass_total[n] = sum(mass);\n",
    "    moments[n] = meta_lognormal_implied_moments(\n",
    "      {mu, sigma}, 0, cutoff[n], 1, 1, 0, 0, 1, primary_params,\n",
    "      accrual[n], 0\n",
    "    );\n  }\n}\n"
  )))
  fit <- mod$sample(
    data = list(
      N = n_case, cutoff = cutoff, accrual = accrual, mu = meanlog,
      sigma = sdlog
    ),
    fixed_param = TRUE, chains = 1, iter_sampling = 1, iter_warmup = 0,
    refresh = 0, show_messages = FALSE
  )
  draws <- posterior::as_draws_matrix(fit$draws())
  stan_moments <- vapply(
    seq_len(n_case),
    function(n) {
      return(as.numeric(draws[1, paste0("moments[", n, ",", 1:4, "]")]))
    },
    numeric(4)
  )
  expect_true(all(is.finite(stan_moments)))
  # The grid Stan built must still be a probability mass function.
  expect_gte(
    min(as.numeric(draws[1, paste0("mass_min[", seq_len(n_case), "]")])), 0
  )
  expect_equal(
    as.numeric(draws[1, paste0("mass_total[", seq_len(n_case), "]")]),
    rep(1, n_case),
    tolerance = 1e-9
  )
  r_moments <- vapply(
    seq_len(n_case),
    function(n) {
      return(unname(.meta_implied_moments(
        "plnorm", list(meanlog = meanlog, sdlog = sdlog),
        cutoff = cutoff[n], pwindow = 1, swindow = 1, trunc_adjusted = 0L,
        cens_adjusted = 0L, growth_rate = 0, trunc_design = accrual[n]
      )))
    },
    numeric(4)
  )
  # The mean and standard deviation are what a study reports. The higher
  # moments only set the sampling standard errors, and weight the grid tail
  # heavily enough to see the difference between summing the cells on the log
  # scale as Stan does and on the delay scale as R does.
  expect_equal(stan_moments[1:2, ], r_moments[1:2, ], tolerance = 1e-6)
  expect_equal(stan_moments, r_moments, tolerance = 1e-3)
})

test_that("epidist.epidist_meta_model recovers known parameters from simulated grid summaries", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Every study reports integer date differences from a right truncated
  # cohort, so the Stan grid branch carries all of the likelihood.
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_true(all(prep_meta_grid$cens_adjusted == 0L))
  expect_true(all(prep_meta_grid$trunc_design == 0L))
  expect_true(all(prep_meta_grid$trunc_adjusted == 0L))
  expect_true(any(prep_meta_grid$obs_type == 5L))
  expect_true(any(prep_meta_grid$obs_type == 6L & prep_meta_grid$group_len > 1))

  expect_convergence(fit_meta_grid)

  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_grid)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.05)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
  expect_lt(stats::quantile(pred$mu, 0.025, names = FALSE), meanlog)
  expect_gt(stats::quantile(pred$mu, 0.975, names = FALSE), meanlog)
  expect_lt(stats::quantile(pred$sigma, 0.025, names = FALSE), sdlog)
  expect_gt(stats::quantile(pred$sigma, 0.975, names = FALSE), sdlog)
})

test_that("epidist.epidist_meta_model recovers known parameters from reported fits and posterior draws", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Five studies published lognormal parameters fitted to their own naive
  # date differences, each summary carrying its own standard error. A sixth
  # published posterior draws of the delay mean and standard deviation, so
  # only that study contributes a covariance.
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_convergence(fit_meta_reported)
  expect_named(.estimates_vcov(sim_reported_estimates), "posterior_draws")
  expect_true(all(
    !is.na(sim_reported_estimates$se) |
      sim_reported_estimates$mvn_id == "posterior_draws"
  ))
  expect_identical(
    unique(sim_reported_estimates$type), c("mean", "sd")
  )

  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_reported)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.05)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
  expect_lt(stats::quantile(pred$mu, 0.025, names = FALSE), meanlog)
  expect_gt(stats::quantile(pred$mu, 0.975, names = FALSE), meanlog)
  expect_lt(stats::quantile(pred$sigma, 0.025, names = FALSE), sdlog)
  expect_gt(stats::quantile(pred$sigma, 0.975, names = FALSE), sdlog)
})

test_that("as_epidist_multivariate round trips draws of a fitted model", {
  # An analyst publishes draws of the delay mean and standard deviation from
  # a fitted model, and those become a summary row of a downstream meta model.
  skip_on_cran()
  skip_if_no_cmdstanr()
  dpars <- predict_delay_parameters(fit_marginal)
  dpars <- dpars[dpars$index == 1, ]
  reported <- as_epidist_multivariate(dpars, params = c("mean", "sd"))
  expect_identical(reported$params, c("mean", "sd"))
  expect_equal(
    unname(reported$value[1]), mean(dpars$mean), tolerance = 1e-10
  )
  estimates <- suppressMessages(as_epidist_estimates_data(
    reported, study = "round_trip", cens_adjusted = 1
  ))
  expect_identical(estimates$type, c("mean", "sd"))
  prep <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_s3_class(prep, "epidist_meta_model")
  # The two summaries are one multivariate normal observation.
  expect_identical(nrow(prep), 1L)
  expect_identical(prep$obs_type, 7L)
  standata <- suppressMessages(epidist(prep, fn = brms::make_standata))
  expect_length(standata$meta_group_chol, 4L)
  # The reported mean must sit close to the truth, because the study that
  # produced these draws adjusted for censoring and truncation.
  expect_equal(
    unname(reported$value[1]), exp(meanlog + sdlog^2 / 2), tolerance = 0.1
  )
})
