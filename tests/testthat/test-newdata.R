test_that("epidist_newdata errors for objects that are not epidist data", {
  expect_error(
    epidist_newdata(data.frame(x = 1)),
    "No .* method is available"
  )
  expect_error(
    epidist_newdata(sim_obs),
    "as_epidist_latent_model"
  )
})

test_that("epidist_newdata.epidist_latent_model returns one row of defaults", {
  newdata <- epidist_newdata(prep_obs)
  expect_s3_class(newdata, "tbl_df")
  expect_false(inherits(newdata, "epidist_latent_model"))
  expect_identical(nrow(newdata), 1L)
  expect_named(
    newdata,
    c("delay", "relative_obs_time", "pwindow", "swindow")
  )
  expect_true(is.na(newdata$delay))
  expect_identical(newdata$relative_obs_time, Inf)
  expect_identical(newdata$pwindow, 0)
  expect_identical(newdata$swindow, 0)
})

test_that("epidist_newdata.epidist_latent_model expands variables", {
  newdata <- epidist_newdata(prep_obs_sex, sex)
  expect_identical(nrow(newdata), 2L)
  expect_identical(sort(newdata$sex), sort(unique(sim_obs_sex$sex)))
  expect_named(
    newdata,
    c("sex", "delay", "relative_obs_time", "pwindow", "swindow")
  )
})

test_that("epidist_newdata.epidist_latent_model sets the windows given", {
  newdata <- epidist_newdata(
    prep_obs,
    pwindow = 1,
    swindow = 1,
    relative_obs_time = 20
  )
  expect_identical(newdata$pwindow, 1)
  expect_identical(newdata$swindow, 1)
  expect_identical(newdata$relative_obs_time, 20)
})

test_that("epidist_newdata.epidist_marginal_model returns the model columns", {
  newdata <- epidist_newdata(prep_marginal_obs)
  expect_s3_class(newdata, "tbl_df")
  expect_identical(nrow(newdata), 1L)
  expect_named(
    newdata,
    c(
      "delay_lwr", "relative_obs_time", "pwindow", "swindow",
      "delay_upr", "delay_min"
    )
  )
  expect_identical(newdata$delay_lwr, 0)
  expect_identical(newdata$delay_upr, 0)
  expect_identical(newdata$delay_min, 0)
  expect_identical(newdata$relative_obs_time, Inf)
})

test_that("epidist_newdata.epidist_marginal_model expands variables", {
  newdata <- epidist_newdata(prep_marginal_obs_sex, sex, pwindow = 1)
  expect_identical(nrow(newdata), 2L)
  expect_identical(unique(newdata$pwindow), 1)
})

test_that("epidist_newdata.epidist_naive_model returns only the response", {
  newdata <- epidist_newdata(prep_naive_obs)
  expect_identical(nrow(newdata), 1L)
  expect_named(newdata, "delay")
  expect_true(is.na(newdata$delay))
})

test_that("epidist_newdata crosses vectors given to named arguments", {
  newdata <- epidist_newdata(
    prep_obs_sex,
    sex,
    relative_obs_time = c(10, 20)
  )
  expect_identical(nrow(newdata), 4L)
  expect_identical(sort(unique(newdata$relative_obs_time)), c(10, 20))
})

test_that("epidist_newdata keeps the values of an expanded model variable", {
  newdata <- epidist_newdata(prep_obs, pwindow)
  expect_identical(
    sort(unique(newdata$pwindow)),
    sort(unique(prep_obs$pwindow))
  )
})

test_that("epidist_newdata errors when a variable is expanded and named", {
  expect_error(
    epidist_newdata(prep_obs, pwindow, pwindow = 1),
    "expanded"
  )
  expect_error(
    epidist_newdata(prep_marginal_obs, delay_min, delay_min = 1),
    "expanded"
  )
})

test_that("epidist_newdata sets a column given with the tidyr syntax", {
  expect_identical(epidist_newdata(prep_obs, delay = 5)$delay, 5)
  expect_identical(epidist_newdata(prep_naive_obs, delay = 5)$delay, 5)
  expect_identical(
    epidist_newdata(prep_marginal_obs, delay_lwr = 5)$delay_lwr,
    5
  )
})

test_that("epidist_newdata checks its numeric arguments", {
  expect_error(epidist_newdata(prep_obs, pwindow = -1), "not >= 0")
  expect_error(epidist_newdata(prep_marginal_obs, delay_min = -1), "not >= 0")
  expect_error(epidist_newdata(prep_meta_obs, swindow = -1), "not >= 0")
  expect_error(
    epidist_newdata(prep_obs, relative_obs_time = NA),
    "missing values"
  )
})

test_that("epidist_newdata works with brms and tidybayes", {
  skip_on_cran()
  skip_if_no_cmdstanr()
  newdata <- epidist_newdata(prep_obs_sex, sex)
  expect_no_error(suppressWarnings(brms::validate_newdata(newdata, fit_sex)))
  epred <- suppressWarnings(
    tidybayes::add_epred_draws(newdata, fit_sex, ndraws = 5)
  )
  expect_identical(nrow(epred), 10L)
  expect_true(all(epred$.epred > 0))
  pred <- suppressWarnings(delay_parameter_draws(fit_sex, newdata = newdata))
  expect_length(unique(pred$.row), 2L)
})

test_that("epidist_newdata works with the marginal model and tidybayes", {
  skip_on_cran()
  skip_if_no_cmdstanr()
  newdata <- epidist_newdata(prep_marginal_obs_sex, sex)
  expect_no_error(suppressWarnings(
    brms::validate_newdata(newdata, fit_marginal_sex)
  ))
  epred <- suppressWarnings(
    tidybayes::add_epred_draws(newdata, fit_marginal_sex, ndraws = 5)
  )
  expect_identical(nrow(epred), 10L)
  expect_true(all(epred$.epred > 0))
  pred <- suppressWarnings(
    delay_parameter_draws(fit_marginal_sex, newdata = newdata)
  )
  expect_length(unique(pred$.row), 2L)
})

test_that("epidist_newdata works with the naive model and tidybayes", {
  skip_on_cran()
  skip_if_no_cmdstanr()
  newdata <- epidist_newdata(prep_naive_obs)
  expect_no_error(brms::validate_newdata(newdata, fit_naive))
  epred <- tidybayes::add_epred_draws(newdata, fit_naive, ndraws = 5)
  expect_identical(nrow(epred), 5L)
  expect_true(all(epred$.epred > 0))
})

test_that("epidist_newdata.epidist_meta_model returns an individual level row", { # nolint: line_length_linter.
  newdata <- epidist_newdata(prep_meta_obs)
  expect_s3_class(newdata, "tbl_df")
  expect_false(inherits(newdata, "epidist_meta_model"))
  expect_identical(nrow(newdata), 1L)
  expect_named(newdata, setdiff(.meta_required_cols(), "n"))
  expect_identical(newdata$delay_lwr, 0)
  expect_identical(newdata$delay_upr, 0)
  expect_identical(newdata$delay_min, 0)
  expect_identical(newdata$relative_obs_time, Inf)
  expect_identical(newdata$pwindow, 0)
  expect_identical(newdata$swindow, 0)
  expect_identical(newdata$obs_type, 1L)
  expect_identical(newdata$study_n, 0L)
  expect_identical(newdata$trunc_adjusted, 0L)
  expect_identical(newdata$trunc_design, 0L)
  expect_identical(newdata$cens_adjusted, 0L)
  expect_identical(newdata$group_start, 1L)
  expect_identical(newdata$group_len, 0L)
  expect_identical(newdata$chol_start, 1L)
  expect_identical(newdata$n_quad, .meta_n_quad())
  expect_identical(newdata$report_se, 0)
  expect_identical(newdata$quantile_p, 0)
  expect_identical(newdata$growth_rate, 0)
})

test_that("epidist_newdata.epidist_meta_model works for summaries alone", {
  newdata <- epidist_newdata(
    prep_meta_estimates,
    pwindow = 1,
    swindow = 1,
    relative_obs_time = 20,
    delay_min = 2
  )
  expect_identical(nrow(newdata), 1L)
  expect_identical(newdata$obs_type, 1L)
  expect_identical(newdata$pwindow, 1)
  expect_identical(newdata$swindow, 1)
  expect_identical(newdata$relative_obs_time, 20)
  expect_identical(newdata$delay_min, 2)
})

test_that("epidist_newdata.epidist_meta_model expands the study", {
  newdata <- epidist_newdata(prep_meta_obs, study)
  expect_identical(nrow(newdata), length(unique(prep_meta_obs$study)))
  expect_setequal(newdata$study, unique(prep_meta_obs$study))
  expect_true(all(newdata$obs_type == 1L))
  expect_true(all(newdata$study_n == 0L))
})

test_that("epidist_newdata works with the meta model and tidybayes", {
  skip_on_cran()
  skip_if_no_cmdstanr()
  newdata <- epidist_newdata(prep_meta_biased)
  expect_no_error(suppressWarnings(
    brms::validate_newdata(newdata, fit_meta_estimates)
  ))
  epred <- suppressWarnings(
    tidybayes::add_epred_draws(newdata, fit_meta_estimates, ndraws = 5)
  )
  expect_identical(nrow(epred), 5L)
  expect_true(all(epred$.epred > 0))
  pred <- suppressWarnings(
    delay_parameter_draws(fit_meta_estimates, newdata = newdata)
  )
  expect_length(unique(pred$.row), 1L)
  summaries <- add_summaries(pred)
  expect_true(all(summaries$mean > 0))
  expect_true(all(summaries$sd > 0))

  newdata <- epidist_newdata(prep_meta_mixed, pwindow = 1, swindow = 1)
  pred <- suppressWarnings(
    delay_parameter_draws(fit_meta_mixed, newdata = newdata)
  )
  expect_length(unique(pred$.row), 1L)
  expect_true(all(add_summaries(pred)$mean > 0))
})
