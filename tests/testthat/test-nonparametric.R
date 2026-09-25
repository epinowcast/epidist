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

test_that("epidist_family() sets default boundaries from the marginal data", {
  family <- epidist_family(prep_marginal_obs, family = nonparametric())
  expect_s3_class(family, "customfamily")
  expect_identical(family$name, "marginal_discretehazard_rw")
  longest <- max(prep_marginal_obs$delay_upr)
  expect_identical(family$np$boundaries, as.numeric(seq(-1, longest)))
  expect_identical(
    family$dpars,
    c("mu", "hsigma", paste0("h", 2:longest, "eps"))
  )
  expect_match(family$param, "epidist_np_params\\(\\{-1.0, 0.0, 1.0")
  expect_match(family$param, "mu, hsigma, \\{h2eps, h3eps")
})

test_that("epidist_family() rejects the nonparametric family elsewhere", {
  expect_error(
    epidist_family(prep_obs, family = nonparametric()),
    "marginal and meta"
  )
  expect_error(
    epidist_family(
      as_epidist_naive_model(sim_obs),
      family = nonparametric()
    ),
    "marginal and meta"
  )
})

test_that("epidist_family() rejects boundaries short of the observed delays", {
  expect_error(
    epidist_family(prep_marginal_obs, family = nonparametric(-1:3)),
    "longest observed delay"
  )
})

test_that("the marginal nonparametric Stan code uses the hazard likelihood", {
  code <- suppressMessages(epidist(
    prep_marginal_obs,
    family = nonparametric(boundaries = -1:25),
    fn = brms::make_stancode
  ))
  code <- as.character(code)
  expect_match(code, "primarycensored_lpmf(\n      y | 27,", fixed = TRUE)
  expect_match(code, "array[] real epidist_np_params(", fixed = TRUE)
  expect_match(
    code,
    "marginal_discretehazard_rw_lpmf(Y[n] | mu[n], hsigma[n], h2eps[n]",
    fixed = TRUE
  )
  skip_on_cran()
  expect_no_error(rstan::stanc(model_code = code))
})

test_that("the random effect hazard model passes its flag to Stan", {
  code <- suppressMessages(epidist(
    prep_marginal_obs,
    family = nonparametric(boundaries = -1:25, hazard_model = "re"),
    fn = brms::make_stancode
  ))
  expect_match(as.character(code), "y | 28,", fixed = TRUE)
  expect_match(as.character(code), "h25eps}, 0)", fixed = TRUE)
})

test_that("the nonparametric family sets its default priors", {
  family <- epidist_family(prep_marginal_obs, nonparametric(-1:25))
  formula <- epidist_formula(prep_marginal_obs, family, formula = mu ~ 1)
  prior <- suppressMessages(
    epidist_prior(prep_marginal_obs, family, formula, prior = NULL)
  )
  intercept <- prior[prior$class == "Intercept", ]
  expect_identical(intercept$prior[intercept$dpar == ""], "normal(0, 1.5)")
  expect_identical(
    intercept$prior[intercept$dpar == "hsigma"], "normal(0, 1)"
  )
  eps <- intercept$prior[grepl("eps$", intercept$dpar)]
  expect_length(eps, 24)
  expect_true(all(eps == "std_normal()"))
})
