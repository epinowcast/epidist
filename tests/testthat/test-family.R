prep_obs <- as_latent_individual(sim_obs)
prep_obs_gamma <- as_latent_individual(sim_obs_gamma)

test_that("epidist_family with default settings produces an object of the right class", { # nolint: line_length_linter.
  family <- epidist_family(prep_obs)
  expect_s3_class(family, "customfamily")
  expect_s3_class(family, "brmsfamily")
  expect_s3_class(family, "family")
})

test_that("epidist_family gives an error when passed inappropriate family input", { # nolint: line_length_linter.
  expect_error(epidist_family(prep_obs, family = 1))
  expect_error(epidist_family(prep_obs, family = list()))
})

test_that("the family argument in epidist_family passes as expected for brms and stats family objects, as well as strings", { # nolint: line_length_linter.
  family_lognormal <- epidist_family(prep_obs, family = brms::lognormal())
  expect_equal(family_lognormal$name, "latent_lognormal")
  family_gamma <- epidist_family(prep_obs, family = Gamma(link = "log"))
  expect_equal(family_gamma$name, "latent_gamma")
  string_lognormal <- epidist_family(prep_obs, family = "lognormal")
  expect_equal(string_lognormal$name, "latent_lognormal")
})

test_that("epidist_family contains the correct reparameterisations for lognormal (no change) and gamma (a change)", { # nolint: line_length_linter.
  family_lognormal <- epidist_family(prep_obs, family = "lognormal")
  expect_equal(family_lognormal$reparam, c("mu", "sigma"))
  family_gamma <- epidist_family(prep_obs, family = Gamma(link = "log"))
  expect_equal(family_gamma$reparam, c("shape", "shape ./ mu"))
})
