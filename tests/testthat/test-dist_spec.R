test_that(".dist_spec_family maps the lognormal family to LogNormal", {
  family <- .dist_spec_family("lognormal")
  expect_identical(family$name, "lognormal")
  expect_identical(family$constructor, "LogNormal")
  expect_identical(family$dpars, c("mu", "sigma"))
  natural <- family$natural(list(mu = c(1.8, 2), sigma = c(0.5, 0.4)))
  expect_identical(natural, list(meanlog = c(1.8, 2), sdlog = c(0.5, 0.4)))
})

test_that(".dist_spec_family converts the gamma rate per draw", {
  family <- .dist_spec_family("gamma")
  expect_identical(family$constructor, "Gamma")
  expect_identical(family$dpars, c("mu", "shape"))
  natural <- family$natural(list(mu = c(6, 8), shape = c(2, 4)))
  expect_identical(natural, list(shape = c(2, 4), rate = c(2 / 6, 4 / 8)))
})

test_that(".dist_spec_family converts the weibull scale per draw", {
  family <- .dist_spec_family("weibull")
  expect_identical(family$constructor, "Weibull")
  expect_identical(family$dpars, c("mu", "shape"))
  natural <- family$natural(list(mu = c(6, 8), shape = c(2, 3)))
  expect_identical(natural$shape, c(2, 3))
  expect_equal(
    natural$scale,
    c(6 / gamma(1 + 1 / 2), 8 / gamma(1 + 1 / 3)),
    tolerance = 1e-8
  )
})

test_that(".dist_spec_family errors for an unsupported family", {
  expect_error(
    .dist_spec_family("beta"),
    "\"beta\" family cannot be exported"
  )
  expect_error(
    .dist_spec_family("beta"),
    "\"lognormal\", \"gamma\", and \"weibull\""
  )
})

test_that(".stratum_names uses the columns that differ between rows", {
  newdata <- data.frame(
    sex = c("Female", "Male", "Female", "Male"),
    age = c(1, 1, 2, 2),
    pwindow = 0,
    stringsAsFactors = FALSE
  )
  expect_identical(
    .stratum_names(newdata),
    c(
      "sex=Female, age=1",
      "sex=Male, age=1",
      "sex=Female, age=2",
      "sex=Male, age=2"
    )
  )
  expect_identical(.stratum_names(newdata[1:2, ]), c("sex=Female", "sex=Male"))
  expect_identical(.stratum_names(newdata[c(1, 1), ]), c("1", "2"))
})

test_that(".dist_spec_from_draws summarises the marginal posteriors", {
  skip_if_not_installed("distspec")
  draws <- data.frame(mu = c(1.7, 1.8, 1.9, 2.0), sigma = c(0.4, 0.5, 0.6, 0.5))
  dist <- .dist_spec_from_draws(draws, .dist_spec_family("lognormal"))
  expect_s3_class(dist, "dist_spec")
  expect_identical(distspec::get_distribution(dist), "lognormal")
  expect_true(distspec::has_uncertainty(dist))
  params <- distspec::get_parameters(dist)
  expect_named(params, c("meanlog", "sdlog"))
  expect_identical(mean(params$meanlog), mean(draws$mu))
  expect_identical(distspec::sd(params$meanlog), stats::sd(draws$mu))
  expect_identical(mean(params$sdlog), mean(draws$sigma))
  expect_identical(distspec::sd(params$sdlog), stats::sd(draws$sigma))
})

test_that(".dist_spec_from_draws converts gamma draws before summarising", {
  skip_if_not_installed("distspec")
  draws <- data.frame(mu = c(5, 6, 7, 8), shape = c(2, 3, 2, 3))
  dist <- .dist_spec_from_draws(draws, .dist_spec_family("gamma"))
  expect_identical(distspec::get_distribution(dist), "gamma")
  params <- distspec::get_parameters(dist)
  expect_named(params, c("shape", "rate"))
  expect_identical(mean(params$rate), mean(draws$shape / draws$mu))
  expect_identical(distspec::sd(params$rate), stats::sd(draws$shape / draws$mu))
})

test_that(".dist_spec_from_draws passes the bounds on", {
  skip_if_not_installed("distspec")
  draws <- data.frame(mu = c(1.7, 1.8, 1.9, 2.0), sigma = c(0.4, 0.5, 0.6, 0.5))
  dist <- .dist_spec_from_draws(
    draws,
    .dist_spec_family("lognormal"),
    max = 30,
    cdf_max = 0.999
  )
  fixed <- distspec::fix_parameters(dist, strategy = "mean")
  expect_identical(as.numeric(max(fixed)), 30)
})

test_that(".dist_spec_from_draws needs at least two draws", {
  skip_if_not_installed("distspec")
  draws <- data.frame(mu = 1.8, sigma = 0.5)
  expect_error(
    .dist_spec_from_draws(draws, .dist_spec_family("lognormal")),
    "At least two draws"
  )
})

test_that(".dist_spec_from_draws uses or refuses the joint representation", {
  skip_if_not_installed("distspec")
  draws <- data.frame(mu = c(1.7, 1.8, 1.9, 2.0), sigma = c(0.4, 0.5, 0.6, 0.5))
  family <- .dist_spec_family("lognormal")
  if (!.has_multi_normal()) {
    expect_error(
      .dist_spec_from_draws(draws, family, representation = "joint"),
      "MultiNormal"
    )
    return(invisible(NULL))
  }
  dist <- .dist_spec_from_draws(draws, family, representation = "joint")
  expect_s3_class(dist, "dist_spec")
  expect_true(distspec::has_uncertainty(dist))
  params <- distspec::get_parameters(dist)
  expect_named(params, c("meanlog", "sdlog"))
  expect_identical(params$meanlog, params$sdlog)
})

test_that(".joint_prior builds one MultiNormal from the mean and covariance", {
  skip_if_not_installed("distspec")
  skip_if_not(.has_multi_normal(), "distspec does not provide MultiNormal")
  natural <- list(
    meanlog = c(1.7, 1.8, 1.9, 2.0),
    sdlog = c(0.4, 0.5, 0.6, 0.5)
  )
  params <- .joint_prior(natural)
  expect_named(params, c("meanlog", "sdlog"))
  expect_identical(params$meanlog, params$sdlog)
  joint <- distspec::get_parameters(params$meanlog)
  expect_identical(
    joint$mean,
    c(meanlog = mean(natural$meanlog), sdlog = mean(natural$sdlog))
  )
  expect_identical(joint$sigma, stats::cov(do.call(cbind, natural)))
})

test_that("epidist_dist_spec exports a latent lognormal fit", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  dist <- epidist_dist_spec(fit)
  expect_s3_class(dist, "dist_spec")
  expect_identical(distspec::get_distribution(dist), "lognormal")
  expect_true(distspec::has_uncertainty(dist))
  draws <- delay_parameter_draws(fit, newdata = epidist_newdata(prep_obs))
  params <- distspec::get_parameters(dist)
  expect_equal(mean(params$meanlog), mean(draws$mu), tolerance = 1e-8)
  expect_equal(
    distspec::sd(params$meanlog),
    stats::sd(draws$mu),
    tolerance = 1e-8
  )
  expect_equal(mean(params$sdlog), mean(draws$sigma), tolerance = 1e-8)
  expect_equal(
    distspec::sd(params$sdlog),
    stats::sd(draws$sigma),
    tolerance = 1e-8
  )
})

test_that("epidist_dist_spec is the distspec::as_dist_spec method", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_identical(distspec::as_dist_spec(fit), epidist_dist_spec(fit))
  expect_identical(
    distspec::as_dist_spec(fit, max = 40),
    epidist_dist_spec(fit, max = 40)
  )
})

test_that("epidist_dist_spec converts the gamma rate per draw", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  dist <- epidist_dist_spec(fit_gamma)
  expect_identical(distspec::get_distribution(dist), "gamma")
  draws <- delay_parameter_draws(
    fit_gamma,
    newdata = epidist_newdata(prep_obs_gamma)
  )
  params <- distspec::get_parameters(dist)
  expect_equal(mean(params$shape), mean(draws$shape), tolerance = 1e-8)
  expect_equal(
    mean(params$rate),
    mean(draws$shape / draws$mu),
    tolerance = 1e-8
  )
  expect_equal(
    distspec::sd(params$rate),
    stats::sd(draws$shape / draws$mu),
    tolerance = 1e-8
  )
})

test_that("epidist_dist_spec exports a marginal weibull fit", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  dist <- epidist_dist_spec(fit_marginal_weibull, max = 50)
  expect_identical(distspec::get_distribution(dist), "weibull")
  draws <- delay_parameter_draws(
    fit_marginal_weibull,
    newdata = epidist_newdata(prep_marginal_obs_weibull)
  )
  params <- distspec::get_parameters(dist)
  scale <- draws$mu / gamma(1 + 1 / draws$shape)
  expect_equal(mean(params$shape), mean(draws$shape), tolerance = 1e-8)
  expect_equal(mean(params$scale), mean(scale), tolerance = 1e-8)
  expect_equal(distspec::sd(params$scale), stats::sd(scale), tolerance = 1e-8)
  fixed <- distspec::fix_parameters(dist, strategy = "mean")
  expect_identical(as.numeric(max(fixed)), 50)
})

test_that("epidist_dist_spec returns a named list for several strata", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  dists <- epidist_dist_spec(fit_sex)
  expect_type(dists, "list")
  expect_named(dists, c("sex=0", "sex=1"))
  expect_s3_class(dists[["sex=0"]], "dist_spec")
  expect_s3_class(dists[["sex=1"]], "dist_spec")
  newdata <- epidist_newdata(prep_obs_sex, sex)
  draws <- delay_parameter_draws(fit_sex, newdata = newdata)
  by_sex <- split(draws, draws$sex)
  for (i in seq_along(dists)) {
    params <- distspec::get_parameters(dists[[i]])
    expect_equal(mean(params$meanlog), mean(by_sex[[i]]$mu), tolerance = 1e-8)
    expect_equal(mean(params$sdlog), mean(by_sex[[i]]$sigma), tolerance = 1e-8)
  }
  expect_identical(
    epidist_dist_spec(fit_sex, newdata = newdata[2, ]),
    dists[["sex=1"]]
  )
})

test_that("epidist_dist_spec passes arguments on to delay_parameter_draws", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  dist <- epidist_dist_spec(fit_marginal, draw_ids = 1:100)
  draws <- delay_parameter_draws(
    fit_marginal,
    newdata = epidist_newdata(prep_marginal_obs),
    draw_ids = 1:100
  )
  params <- distspec::get_parameters(dist)
  expect_equal(mean(params$meanlog), mean(draws$mu), tolerance = 1e-8)
  expect_equal(
    distspec::sd(params$meanlog),
    stats::sd(draws$mu),
    tolerance = 1e-8
  )
})

test_that("epidist_dist_spec errors for an unsupported family", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  fit_beta <- fit
  fit_beta$family$name <- "latent_beta"
  expect_error(
    epidist_dist_spec(fit_beta),
    "\"beta\" family cannot be exported"
  )
})

test_that("epidist_dist_spec uses or refuses the joint representation", {
  skip_if_not_installed("distspec")
  skip_on_cran()
  skip_if_no_cmdstanr()
  if (!.has_multi_normal()) {
    expect_error(
      epidist_dist_spec(fit, representation = "joint"),
      "MultiNormal"
    )
    return(invisible(NULL))
  }
  dist <- epidist_dist_spec(fit, representation = "joint")
  expect_s3_class(dist, "dist_spec")
  params <- distspec::get_parameters(dist)
  expect_identical(params$meanlog, params$sdlog)
})

test_that("epidist_dist_spec only accepts a fitted model", {
  skip_if_not_installed("distspec")
  expect_error(epidist_dist_spec(prep_obs), "epidist_fit")
})
