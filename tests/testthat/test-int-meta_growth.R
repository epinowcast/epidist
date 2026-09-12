# fmt: skip file
# Summary estimates that all leave their growth rate to be estimated, one
# study per observation type, so that the growth rate reaches every branch of
# the Stan likelihood as a parameter rather than as data.
growth_all_types <- suppressMessages(as_epidist_estimates_data(list(
  data.frame(
    study = c(
      "mean_only", "sd_only", "quantile_se", "pair", "pair",
      "quantiles", "quantiles", "mixed", "mixed"
    ),
    type = c(
      "mean", "sd", "quantile", "mean", "sd", "quantile", "quantile",
      "mean", "quantile"
    ),
    value = c(7.5, 3.6, 6.2, 7.4, 3.5, 4.8, 8.6, 7.2, 6.8),
    se = c(0.4, 0.3, 0.5, NA, NA, NA, NA, NA, NA),
    p = c(NA, NA, 0.5, NA, NA, 0.25, 0.75, NA, 0.5),
    n = c(120, 120, 70, 115, 115, 60, 60, 90, 90),
    relative_obs_time = c(20, 22, 24, 26, 26, 30, 30, 28, 28),
    trunc_adjusted = FALSE,
    trunc_design = "accrual",
    cens_adjusted = c(0, 0, 0, 0, 0, 0, 0, 2, 2),
    growth_rate = NA,
    growth_rate_sd = NA,
    stringsAsFactors = FALSE
  ),
  suppressMessages(as_epidist_estimates_data(
    new_epidist_multivariate(
      value = c(mean = 7.2, sd = 3.4, q0.5 = 6.5),
      vcov = matrix(
        c(0.09, 0.02, 0.01, 0.02, 0.16, 0.03, 0.01, 0.03, 0.25),
        nrow = 3
      ),
      params = c("mean", "sd", "q0.5")
    ),
    study = "covariance",
    relative_obs_time = 30,
    trunc_adjusted = FALSE,
    trunc_design = "accrual",
    cens_adjusted = 0,
    delay_min = 0,
    growth_rate = 0.1,
    growth_rate_sd = 0.02
  ))
)))

test_that("the meta model Stan code is accepted where a summary row estimates its growth rate", { # nolint: line_length_linter.
  skip_on_cran()
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = growth_all_types)
  )
  expect_true(.meta_growth_estimated(meta))
  expect_true(all(meta$growth_known == 0L))
  expect_true("pgrowth" %in% epidist_family(meta)$dpars)
  # The rate the likelihood uses is a parameter here, so no function it
  # reaches may qualify its growth rate argument as data only. A function
  # that does is rejected, naming the argument that must be data-only, and
  # only `meta_family_lpmf()` may take the reported slot as data.
  stancode <- suppressMessages(epidist(meta, fn = brms::make_stancode))
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("every observation type of the meta model reads the estimated growth rate", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = growth_all_types)
  )
  program <- meta_log_lik_program(meta)
  expect_true(program$growth)
  # Every observation type is fitted here, so no branch is checked
  # vacuously. Extend the fixture where a new observation type is added.
  expect_setequal(as.integer(program$standata$vint1), 2:8)
  # Every row leaves its rate to be estimated, and the slot holds a
  # placeholder rather than the rate in use, so a row reading the slot
  # still has a rate to fit at and would fail quietly.
  expect_true(all(program$standata$vint10 == 0L))
  expect_false(any(program$standata$vreal8 == 0.05))
  base <- meta_stan_log_lik(program, 1.9, 0.5, 0.05)
  moved <- meta_stan_log_lik(program, 1.9, 0.5, 0.2)
  expect_true(all(is.finite(base)))
  expect_true(all(is.finite(moved)))
  # A row that lost the parameter altogether stops responding to it, and
  # the observation type that did is named.
  unmoved <- which(abs(moved - base) <= 1e-6)
  expect_identical(as.integer(program$standata$vint1[unmoved]), integer(0))
  # R resolves the rate from the same slots, so it is the pin on a call
  # site that hands on the slot where the resolved rate is wanted. Such a
  # row can still respond to the parameter through another argument, so
  # this is what catches it rather than the check above.
  expect_rows_close(meta_r_log_lik(program, 1.9, 0.5, 0.05), base, 1e-4)
})
