# fmt: skip file
test_that("Integration with priorsense works as expected", {
  skip_if_not_installed("priorsense")
  skip_on_cran()

  expect_error(
    priorsense::powerscale_sensitivity(
      fit_marginal,
      variable = "Intercept"
    ),
    NA
  )

  expect_error(
    priorsense::powerscale_sensitivity(
      fit_marginal_gamma,
      variable = "Intercept"
    ),
    NA
  )

  expect_error(
    priorsense::powerscale_sensitivity(
      fit_marginal_weibull,
      variable = "Intercept"
    ),
    NA
  )
})

test_that("Integration with marginaleffects works as expected", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("collapse")
  skip_on_cran()

  # Test that avg_comparisons works for stratified models
  expect_error(
    marginaleffects::avg_comparisons(
      fit_sex,
      variables = list(sex = c(0, 1))
    ),
    NA
  )

  expect_error(
    marginaleffects::avg_comparisons(
      fit_marginal_sex,
      variables = list(sex = c(0, 1))
    ),
    NA
  )
})

test_that("Integration with loo works as expected", {
  skip_if_not_installed("loo")
  skip_on_cran()

  suppressWarnings(expect_error(loo::loo(fit_marginal, cores = 2), NA))
  suppressWarnings(expect_error(loo::loo(fit_marginal_gamma, cores = 2), NA))
  suppressWarnings(
    expect_error(
      loo::loo(fit_marginal_weibull, cores = 2),
      NA
    )
  )
})
