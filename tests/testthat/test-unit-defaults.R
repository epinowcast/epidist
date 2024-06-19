# This unit test requires a feature to be implemented in epidist_prepare
# Adding issue to implement this

# test_that("epidist_prepare.default gives an error when passed a list", { # nolint: line_length_linter.
#   expect_error(epidist_prepare(list(), model = "latent_individual"))
# })

test_that("epidist_prepare.default gives an error when passed a model which is not implemented", { # nolint: line_length_linter.
  expect_error(epidist_prepare(sim_obs, model = "not_a_real_model"))
})

test_that("epidist_family.default gives an error when passed a list", { # nolint: line_length_linter.
  expect_error(epidist_family(list()))
})

test_that("epidist_formula.default gives an error when passed a list", { # nolint: line_length_linter.
  expect_error(epidist_formula(list()))
})

test_that("epidist_prior.default gives an error when passed a list", { # nolint: line_length_linter.
  expect_error(epidist_prior(list()))
})

test_that("epidist_standata.default gives an error when passed a list", { # nolint: line_length_linter.
  expect_error(epidist_standata(list()))
})
