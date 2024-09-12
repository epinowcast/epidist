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
