# fmt: skip file
test_that(
  "epidist_family with default settings produces an object of the right class",
  {
    family <- epidist_family(prep_obs)
    expect_s3_class(family, "customfamily")
    expect_s3_class(family, "brmsfamily")
    expect_s3_class(family, "family")
  }
)

test_that(
  "epidist_family gives an error when passed inappropriate family input",
  {
    expect_error(epidist_family(prep_obs, family = 1))
    expect_error(epidist_family(prep_obs, family = list()))
  }
)

test_that(
  "the family argument in epidist_family passes as expected for brms and stats family objects, as well as strings", # nolint: line_length_linter.
  {
    family_lognormal <- epidist_family(prep_obs, family = lognormal())
    expect_identical(family_lognormal$name, "latent_lognormal")
    family_gamma <- epidist_family(prep_obs, family = Gamma(link = "log"))
    expect_identical(family_gamma$name, "latent_gamma")
    string_lognormal <- epidist_family(prep_obs, family = "lognormal")
    expect_identical(string_lognormal$name, "latent_lognormal")
  }
)

test_that(
  "epidist_family contains the correct reparameterisations for lognormal (no change) and gamma (a change)", # nolint: line_length_linter.
  {
    family_lognormal <- epidist_family(prep_obs, family = "lognormal")
    expect_identical(family_lognormal$param, "mu, sigma") # nolint
    family_gamma <- epidist_family(prep_obs, family = Gamma(link = "log"))
    expect_identical(family_gamma$param, "shape, shape ./ mu") # nolint
    family_weibull <- epidist_family(prep_obs, family = "Weibull")
    expect_identical(
      family_weibull$param, "shape, mu ./ tgamma(1 + 1 ./ shape)"
    )
  }
)
