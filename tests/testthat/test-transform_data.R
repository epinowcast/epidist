test_that(
  "epidist_transform_data with default settings returns data unchanged",
  {
    family <- epidist_family(prep_obs, family = lognormal())
    formula <- epidist_formula(prep_obs, family = family, formula = bf(mu ~ 1))

    transformed <- epidist_transform_data(prep_obs, family, formula)
    expect_identical(transformed, prep_obs)
  }
)

test_that("epidist_transform_data errors when passed incorrect inputs", {
  family <- epidist_family(prep_obs, family = lognormal())
  formula <- epidist_formula(prep_obs, family = family, formula = bf(mu ~ 1))

  expect_error(epidist_transform_data(list(), family, formula))
})

test_that("epidist_transform_data_model.default returns data unchanged", {
  family <- epidist_family(prep_obs, family = lognormal())
  formula <- epidist_formula(prep_obs, family = family, formula = bf(mu ~ 1))

  transformed <- epidist_transform_data_model(prep_obs, family, formula)
  expect_identical(transformed, prep_obs)
})

test_that("epidist_transform_data works with different model types", {
  family <- epidist_family(prep_obs, family = lognormal())
  formula <- epidist_formula(prep_obs, family = family, formula = bf(mu ~ 1))

  expect_identical(
    epidist_transform_data(prep_naive_obs, family, formula),
    prep_naive_obs
  )
  expect_identical(
    epidist_transform_data(prep_obs_gamma, family, formula),
    prep_obs_gamma
  )
})
