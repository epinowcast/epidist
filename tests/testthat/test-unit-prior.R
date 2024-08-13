test_that("replace_brms_prior errors when passed a new prior without a match in old_prior", { # nolint: line_length_linter.
  old_prior <- brms::prior("normal(0, 10)", class = "Intercept") +
    brms::prior("normal(0, 10)", class = "Intercept", dpar = "sigma")
  new_prior <- brms::prior("normal(0, 5)", class = "Intercept") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "sigma") +
    brms::prior("normal(0, 5)", class = "Intercept", dpar = "shape")
  expect_error(replace_brms_prior(old_prior, new_prior))
})
