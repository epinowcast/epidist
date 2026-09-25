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

test_that("gengamma's Stacy conversion gives the same distribution", {
  skip_if_not_installed("flexsurv")
  x <- c(0.5, 2, 6, 15)
  mu <- c(1.6, 2, 0.8)
  sigma <- c(0.5, 0.9, 0.3)
  Q <- c(1.1, 0.4, 2)
  for (j in seq_along(mu)) {
    stacy <- .gengamma_stacy(mu[j], sigma[j], Q[j])
    expect_equal(
      flexsurv::dgengamma.orig(x, stacy$shape, stacy$scale, stacy$k),
      flexsurv::dgengamma(x, mu[j], sigma[j], Q[j]),
      tolerance = 1e-10
    )
    expect_equal(
      .gengamma_mean(stacy$scale, stacy$shape, stacy$k),
      flexsurv::mean_gengamma(mu[j], sigma[j], Q[j]),
      tolerance = 1e-8
    )
  }
  # Q = 1 is the weibull with shape 1 / sigma and scale exp(mu)
  expect_equal(
    flexsurv::dgengamma(x, 1.6, 0.5, 1),
    stats::dweibull(x, shape = 2, scale = exp(1.6)),
    tolerance = 1e-10
  )
  # Q = sigma is the gamma with shape 1 / Q^2 and scale exp(mu) * Q^2
  stacy <- .gengamma_stacy(log(6), sqrt(0.5), sqrt(0.5))
  expect_equal(
    flexsurv::dgengamma.orig(x, stacy$shape, stacy$scale, stacy$k),
    stats::dgamma(x, shape = 2, scale = 3),
    tolerance = 1e-10
  )
  expect_equal(
    .gengamma_sd(stacy$scale, stacy$shape, stacy$k), sqrt(2) * 3,
    tolerance = 1e-10
  )
})

test_that("gengamma's brms functions match flexsurv", {
  skip_if_not_installed("flexsurv")
  mu <- c(1.5, 1.7, 2.1)
  sigma <- c(0.5, 0.6, 0.4)
  Q <- c(0.8, 1, 1.3)
  prep <- structure(
    list(
      ndraws = 3L,
      nobs = 2L,
      dpars = list(
        mu = matrix(mu, 3, 2), sigma = matrix(sigma, 3, 2),
        Q = matrix(Q, 3, 2)
      ),
      data = list(Y = c(2, 5))
    ),
    class = "brmsprep"
  )
  expect_equal(
    .gengamma_log_lik(2, prep),
    flexsurv::dgengamma(5, mu, sigma, Q, log = TRUE),
    tolerance = 1e-10
  )
  expect_equal(
    as.vector(.gengamma_posterior_epred(prep)[, 1]),
    flexsurv::mean_gengamma(mu, sigma, Q),
    tolerance = 1e-8
  )
  set.seed(1)
  n <- 20000L
  draws_prep <- structure(
    list(
      ndraws = n, nobs = 1L,
      dpars = list(mu = rep(1.7, n), sigma = rep(0.6, n), Q = rep(1, n)),
      data = list()
    ),
    class = "brmsprep"
  )
  drawn <- .gengamma_posterior_predict(1, draws_prep)
  expect_length(drawn, n)
  expect_equal(
    mean(drawn), flexsurv::mean_gengamma(1.7, 0.6, 1),
    tolerance = 0.02
  )
  expect_equal(
    stats::median(drawn), flexsurv::qgengamma(0.5, 1.7, 0.6, 1),
    tolerance = 0.02
  )
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
