test_that("epidist_prepare.default fails appropriately when passed bad data", { # nolint: line_length_linter.
  prep_obs <- epidist_prepare(list(), model = "latent_individual")
})

test_that("epidist_prepare.default fails appropriately when passed bad model", { # nolint: line_length_linter.
  prep_obs <- epidist_prepare(list(), model = "not_a_real_model")
})

test_that("epidist_family.default warns user that there is no method for a list", { # nolint: line_length_linter.
  family <- epidist_family(list())
})

test_that("epidist_formula.default warns user that there is no method for a list", { # nolint: line_length_linter.
  formula <- epidist_formula(list())
})

test_that("epidist_prior.default warns user that there is no method for a list", { # nolint: line_length_linter.
  prior <- epidist_prior(list())
})

test_that("epidist_standata.default warns user that there is no method for a list", { # nolint: line_length_linter.
  standata <- epidist_standata(list())
})
