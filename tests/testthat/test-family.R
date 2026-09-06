# fmt: skip file
test_that(
  "epidist_family with default settings produces an object of the right class",
  { # nolint: line_length_linter.
    family <- epidist_family(prep_obs)
    expect_s3_class(family, "customfamily")
    expect_s3_class(family, "brmsfamily")
    expect_s3_class(family, "family")
  }
)

test_that(
  "epidist_family gives an error when passed inappropriate family input",
  { # nolint: line_length_linter.
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
    expect_identical(family_weibull$param, "shape, mu ./ tgamma(1 + 1 ./ shape)") # nolint
  }
)

test_that("gengamma builds a brms custom family in the Stacy parameterisation", { # nolint: line_length_linter.
  skip_if_not_installed("flexsurv")
  family <- gengamma()
  expect_s3_class(family, "customfamily")
  expect_identical(family$name, "gengamma")
  expect_identical(family$dpars, c("mu", "shape", "k"))
  expect_identical(family$link, "log")
  expect_identical(family$link_shape, "log")
  expect_identical(family$link_k, "log")
  expect_identical(unname(unlist(family$lb)), c("0", "0", "0"))
  expect_identical(family$ybounds, c(0, Inf))
  expect_type(family$log_lik, "closure")
  expect_type(family$posterior_predict, "closure")
  expect_type(family$posterior_epred, "closure")
})

test_that("epidist_family builds the gengamma family for every model", {
  skip_if_not_installed("flexsurv")
  latent <- epidist_family(prep_obs, family = gengamma())
  expect_s3_class(latent, "gengamma")
  expect_identical(latent$name, "latent_gengamma")
  expect_identical(latent$dpars, c("mu", "shape", "k"))
  expect_identical(latent$param, "mu, shape, k")
  expect_identical(latent$pcd_param, "shape, mu, k")
  expect_identical(latent$link_shape, "log")
  expect_identical(latent$link_k, "log")
  marginal <- epidist_family(prep_marginal_obs, family = gengamma())
  expect_identical(marginal$name, "marginal_gengamma")
  meta <- epidist_family(prep_meta_obs, family = "gengamma")
  expect_identical(meta$name, "meta_gengamma")
  expect_identical(meta$pcd_param, "shape, mu, k")
  # The naive model passes the family to brms unchanged
  naive <- epidist_family(prep_naive_obs, family = gengamma())
  expect_identical(naive$name, "gengamma")
  expect_identical(naive$family, "custom")
  expect_identical(.delay_family(naive)$name, "gengamma")
})

test_that("the gengamma family is looked up by name where brms would be", {
  skip_if_not_installed("flexsurv")
  expect_identical(.validate_family("gengamma")$name, "gengamma")
  expect_identical(.validate_family("gengamma")$link, "log")
  expect_identical(
    .validate_family(c("gengamma", "identity"))$link, "identity"
  )
  expect_identical(
    .validate_family("gengamma", link = "identity")$link, "identity"
  )
  expect_identical(
    epidist_family(prep_obs, family = c("gengamma", "identity"))$link,
    "identity"
  )
  expect_identical(.pcd_family_dist_name(gengamma()), "pgengamma.orig")
  expect_identical(
    .get_brms_fn("posterior_predict", list(family = "gengamma")),
    gengamma()$posterior_predict
  )
  expect_identical(.pdist("pgengamma.orig"), flexsurv::pgengamma.orig)
  expect_true("pgengamma.orig" %in% .get_supported_dists())
})
