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

test_that("gengamma builds a brms custom family in the Prentice parameterisation", { # nolint: line_length_linter.
  skip_if_not_installed("flexsurv")
  family <- gengamma()
  expect_s3_class(family, "customfamily")
  expect_identical(family$name, "gengamma")
  expect_identical(family$dpars, c("mu", "sigma", "Q"))
  expect_identical(family$link, "identity")
  expect_identical(family$link_sigma, "log")
  expect_identical(family$link_Q, "log")
  expect_identical(unname(unlist(family$lb))[2:3], c("0", "0"))
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
  stacy <- "Q / sigma, exp(mu + 2 * sigma * log(Q) / Q), inv_square(Q)"
  expect_identical(latent$dpars, c("mu", "sigma", "Q"))
  expect_identical(latent$param, "mu, sigma, Q")
  expect_identical(latent$pcd_param, stacy)
  expect_identical(latent$link_sigma, "log")
  expect_identical(latent$link_Q, "log")
  marginal <- epidist_family(prep_marginal_obs, family = gengamma())
  expect_identical(marginal$name, "marginal_gengamma")
  meta <- epidist_family(prep_meta_obs, family = "gengamma")
  expect_identical(meta$name, "meta_gengamma")
  expect_identical(meta$pcd_param, stacy)
  # The naive model passes the family to brms unchanged
  naive <- epidist_family(prep_naive_obs, family = gengamma())
  expect_identical(naive$name, "gengamma")
  expect_identical(naive$family, "custom")
  expect_identical(.delay_family(naive)$name, "gengamma")
})

test_that(".pcd_family_dist_name resolves a model-wrapped meta family", { # nolint: line_length_linter.
  # `epidist_family()` returns the family already wrapped by
  # `brms::custom_family()`, so `family$family` is `"custom"` and only
  # `.delay_family()` recovers the underlying distribution name.
  meta <- epidist_family(prep_meta_obs, family = lognormal())
  expect_identical(meta$family, "custom")
  expect_identical(.pcd_family_dist_name(meta), "plnorm")
})

test_that("the gengamma family is looked up by name where brms would be", {
  skip_if_not_installed("flexsurv")
  expect_identical(.validate_family("gengamma")$name, "gengamma")
  expect_identical(.validate_family("gengamma")$link, "identity")
  expect_identical(.validate_family(c("gengamma", "log"))$link, "log")
  expect_identical(.validate_family("gengamma", link = "log")$link, "log")
  expect_identical(
    epidist_family(prep_obs, family = c("gengamma", "log"))$link, "log"
  )
  expect_identical(.pcd_family_dist_name(gengamma()), "pgengamma.orig")
  expect_identical(
    .get_brms_fn("posterior_predict", list(family = "gengamma")),
    gengamma()$posterior_predict
  )
  expect_identical(.pdist("pgengamma.orig"), flexsurv::pgengamma.orig)
  expect_true("pgengamma.orig" %in% .get_supported_dists())
})
