test_that("nonparametric() builds a random walk hazard family by default", {
  family <- nonparametric(boundaries = -1:5)
  expect_s3_class(family, "brmsfamily")
  expect_identical(family$family, "discretehazard_rw")
  expect_identical(family$np$hazard_model, "rw")
  expect_identical(family$np$boundaries, as.numeric(-1:5))
  # Six bins, so four hazards are free after the first and the last
  expect_identical(
    family$dpars,
    c("mu", "hsigma", "h2eps", "h3eps", "h4eps", "h5eps")
  )
  expect_identical(family$link, "identity")
})

test_that("nonparametric() builds a random effect hazard family", {
  family <- nonparametric(boundaries = -1:5, hazard_model = "re")
  expect_identical(family$family, "discretehazard_re")
  expect_identical(
    family$dpars,
    c("mu", "hsigma", "h1eps", "h2eps", "h3eps", "h4eps", "h5eps")
  )
})

test_that("nonparametric() leaves the parameters to the data by default", {
  family <- nonparametric()
  expect_null(family$np$boundaries)
  expect_identical(family$dpars, "mu")
})

test_that("nonparametric() rejects boundaries it cannot use", {
  expect_error(nonparametric(boundaries = c(0, 2, 1, 3)), "increasing")
  expect_error(nonparametric(boundaries = c(-1, 0, 1)), "at least")
  expect_error(nonparametric(boundaries = c(-1, 0, 1, NA)))
  expect_error(nonparametric(hazard_model = "dirichlet"))
})

test_that("the hazard family maps to the primarycensored hazard likelihood", {
  # primarycensored gives the alias "nonparametric" to the direct PMF step,
  # dist_id 26, so the family must not be looked up by that name.
  expect_identical(
    primarycensored::pcd_stan_dist_id(nonparametric(-1:5)$family), 27L
  )
  expect_identical(
    primarycensored::pcd_stan_dist_id(
      nonparametric(-1:5, hazard_model = "re")$family
    ),
    28L
  )
})

test_that(".np_pmf() matches primarycensored for each hazard model", {
  boundaries <- -1:4
  mu <- c(-1, 0.5)
  hsigma <- c(0.5, 1.2)
  eps <- list(h2eps = c(0.3, -0.2), h3eps = c(-1, 0.4), h4eps = c(0.2, 0.1))
  pmf <- .np_pmf(c(list(mu = mu, hsigma = hsigma), eps), boundaries, "rw")
  expect_identical(dim(pmf), c(2L, 5L))
  for (d in 1:2) {
    offset <- c(0, cumsum(unname(vapply(eps, `[`, numeric(1), d))))
    hazards <- c(stats::plogis(mu[d] + hsigma[d] * offset), 1)
    expect_equal(pmf[d, ], primarycensored::hazards_to_pmf(hazards))
  }
  re_eps <- c(list(h1eps = c(0.1, 0.2)), eps)
  pmf_re <- .np_pmf(
    c(list(mu = mu, hsigma = hsigma), re_eps), boundaries, "re"
  )
  offset <- unname(vapply(re_eps, `[`, numeric(1), 1))
  hazards <- c(stats::plogis(mu[1] + hsigma[1] * offset), 1)
  expect_equal(pmf_re[1, ], primarycensored::hazards_to_pmf(hazards))
})
