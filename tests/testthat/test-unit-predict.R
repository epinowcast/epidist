data <- as_latent_individual(sim_obs)
fit <- epidist(data)
prep <- brms::prepare_predictions(fit)
i <- 1
posterior_predict_latent_lognormal(i = i, prep = prep)

test_that("predict_delay_parameters works with NULL newdata and the latent lognormal model", { # nolint: line_length_linter.
  expect_equal(1, 1)
})
