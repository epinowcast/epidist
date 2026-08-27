# fmt: skip file
test_that("epidist.epidist_meta_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
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
  expect_s3_class(fit_meta_estimates, "brmsfit")
  expect_s3_class(fit_meta_estimates, "epidist_fit")
  expect_convergence(fit_meta_estimates)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from biased summary estimates", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_estimates)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.15)
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  expect_s3_class(fit_meta_mixed, "brmsfit")
  expect_s3_class(fit_meta_mixed, "epidist_fit")
  expect_convergence(fit_meta_mixed)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_mixed)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_meta_model log_lik and posterior_predict have the expected shapes", { # nolint: line_length_linter.
  skip_on_cran()
  set.seed(1)
  log_lik <- brms::log_lik(fit_meta_estimates)
  expect_identical(ncol(log_lik), nrow(prep_meta_biased))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_estimates)
  expect_identical(ncol(pred), nrow(prep_meta_biased))
  expect_true(all(is.finite(pred)))
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
  # Every observation type must be exercised, including the three joint ones,
  # and every censoring adjustment, both truncation designs, a left truncated
  # study and a quantile with a reported standard error, so that each branch
  # of the implied density is covered as well.
  expect_setequal(unique(standata$vint1), 2:7)
  expect_setequal(unique(standata$vint4), 0:3)
  expect_setequal(unique(standata$vint5), 0:1)
  expect_true(any(standata$vreal5 > 0))
  expect_true(any(standata$vreal6 > 0))
  expect_equal(stan_log_lik, r_log_lik, tolerance = 1e-6)
})

test_that("epidist.epidist_meta_model recovers known parameters from simulated grid summaries", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Exercises the Stan discrete grid branch rather than the R mirrors. Every
  # study reports integer date differences from a right truncated cohort, so
  # meta_family_grid_pmf and the cohort grid shortcut carry all of the
  # likelihood.
  skip_on_cran()
  expect_true(all(prep_meta_grid$cens_adjusted == 0L))
  expect_true(all(prep_meta_grid$trunc_design == 0L))
  expect_true(all(prep_meta_grid$trunc_adjusted == 0L))
  expect_true(any(prep_meta_grid$obs_type == 5L))
  expect_true(any(prep_meta_grid$obs_type == 6L & prep_meta_grid$group_len > 1))

  expect_convergence(fit_meta_grid)

  set.seed(1)
  pred <- predict_delay_parameters(fit_meta_grid)
  # The posterior mean must sit within 0.05 of the simulation meanlog and
  # within 0.1 of its sdlog, and the central 95% of the posterior must cover
  # both.
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.05)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
  expect_lt(stats::quantile(pred$mu, 0.025, names = FALSE), meanlog)
  expect_gt(stats::quantile(pred$mu, 0.975, names = FALSE), meanlog)
  expect_lt(stats::quantile(pred$sigma, 0.025, names = FALSE), sdlog)
  expect_gt(stats::quantile(pred$sigma, 0.975, names = FALSE), sdlog)
})
