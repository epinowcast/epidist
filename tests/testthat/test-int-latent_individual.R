prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

test_that("epidist.epidist_latent_individual can be used to extract valid Stan code which compiles", { # nolint: line_length_linter.
  stancode <- epidist(data = prep_obs, fn = brms::make_stancode)
  compiled_model <- rstan::stan_model(model_code = stancode)
})

test_that("epidist.epidist_latent_individual fits in the default case", {
  fit <- epidist(data = prep_obs)
  expect_s3_class(fit, "brmsfit")
  expect_s3_class(fit, "epidist_fit")
  expect_convergence(fit)
})

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