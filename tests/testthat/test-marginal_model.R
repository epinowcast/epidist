test_that("as_epidist_marginal_model.epidist_linelist_data with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_epidist_marginal_model(sim_obs)
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_model")
})
