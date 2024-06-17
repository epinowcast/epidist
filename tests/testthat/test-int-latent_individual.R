# Assumes that tests/testthat/setup.R has been run

# Generate observation data in correct format for the latent_individual model
prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

test_that("epidist.epidist_latent_individual fits in the default case", {
  fit <- epidist(data = prep_obs)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

# Could also do tests on the stancode?
stancode_external <- epidist(prep_obs, fn = brms::make_stancode)

test_that("epidist.epidist_latent_individual fits for a gamma delay distribution", { # nolint: line_length_linter.
  fit_gamma <- epidist(
    data = prep_obs,
    family = epidist_family(prep_obs, family = "gamma"),
    stancode = epidist_stancode(
      prep_obs,
      family = epidist_family(prep_obs, family = "gamma")
    )
  )
  expect_s3_class(fit_gamma, "brmsfit")
  expect_s3_class(fit_gamma, "epidist_fit")
  expect_convergence(fit_gamma)
})