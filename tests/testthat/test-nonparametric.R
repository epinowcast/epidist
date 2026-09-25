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
    expect_equal(
      pmf[d, ], primarycensored::hazards_to_pmf(hazards),
      tolerance = 1e-12
    )
  }
  re_eps <- c(list(h1eps = c(0.1, 0.2)), eps)
  pmf_re <- .np_pmf(
    c(list(mu = mu, hsigma = hsigma), re_eps), boundaries, "re"
  )
  offset <- unname(vapply(re_eps, `[`, numeric(1), 1))
  hazards <- c(stats::plogis(mu[1] + hsigma[1] * offset), 1)
  expect_equal(
    pmf_re[1, ], primarycensored::hazards_to_pmf(hazards),
    tolerance = 1e-12
  )
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
  expect_match(
    family$param, "epidist_np_params({-1.0, 0.0, 1.0",
    fixed = TRUE
  )
  expect_match(family$param, "mu, hsigma, {h2eps, h3eps", fixed = TRUE)
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
  # 26 bins, so the hazard of the first is 1 / 26 when all are equally likely
  expect_identical(
    intercept$prior[!nzchar(intercept$dpar)], "normal(-3.22, 1.5)"
  )
  expect_identical(
    intercept$prior[intercept$dpar == "hsigma"], "normal(0, 1)"
  )
  eps <- intercept$prior[endsWith(intercept$dpar, "eps")]
  expect_length(eps, 24)
  expect_true(all(eps == "std_normal()"))
})

np_draws <- data.frame(
  mu = c(-1, -0.5), hsigma = c(0.5, 0.8),
  h2eps = c(0.3, -0.2), h3eps = c(-0.5, 0.4), h4eps = c(0.2, 0.1)
)

test_that("add_summaries() gives the moments and quantiles of the bins", {
  family <- nonparametric(boundaries = -1:4)
  out <- add_summaries(np_draws, family = family, probs = c(0.1, 0.5, 0.99))
  pmf <- .np_pmf(np_draws, -1:4, "rw")
  edges <- as.numeric(0:4)
  expect_equal(out$mean, as.vector(pmf %*% edges), tolerance = 1e-12)
  expect_equal(
    out$sd, sqrt(as.vector(pmf %*% edges^2) - out$mean^2),
    tolerance = 1e-10
  )
  for (row in 1:2) {
    cdf <- cumsum(pmf[row, ])
    expect_identical(out$q50[row], edges[which(cdf >= 0.5)[1]])
    expect_identical(out$q10[row], edges[which(cdf >= 0.1)[1]])
    expect_identical(out$q99[row], edges[which(cdf >= 0.99)[1]])
  }
})

test_that("add_summaries() by simulation agrees with the analytic summaries", {
  family <- nonparametric(boundaries = -1:4)
  analytic <- add_summaries(np_draws, family = family)
  withr::local_seed(1)
  sampled <- add_summaries(
    np_draws,
    family = family, method = "sample", nsim = 20000
  )
  expect_equal(sampled$mean, analytic$mean, tolerance = 0.02)
  expect_equal(sampled$sd, analytic$sd, tolerance = 0.03)
})

test_that("the nonparametric density is the histogram of the bins", {
  summaries <- .np_delay_summaries(nonparametric(-1:4)$np)
  pmf <- .np_pmf(np_draws, -1:4, "rw")
  expect_identical(summaries$density(np_draws, 2.5), pmf[, 4])
  expect_identical(summaries$density(np_draws, 3), pmf[, 4])
  expect_identical(summaries$density(np_draws, 5), c(0, 0))
})

test_that("add_summaries() needs the boundaries of the family", {
  expect_error(
    add_summaries(np_draws, family = nonparametric()),
    "boundaries"
  )
})

np_meta_estimates <- suppressMessages(as_epidist_estimates_data(
  data.frame(
    study = c("A", "A", "B", "B", "C"),
    type = c("mean", "sd", "mean", "sd", "quantile"),
    value = c(7.5, 3.6, 6.4, 3.1, 5.4),
    p = c(NA, NA, NA, NA, 0.5),
    n = c(120, 120, 80, 80, 200),
    relative_obs_time = c(20, 20, Inf, Inf, 30),
    trunc_adjusted = c(FALSE, FALSE, TRUE, TRUE, FALSE),
    cens_adjusted = c(0, 0, 1, 1, 2),
    stringsAsFactors = FALSE
  )
))
np_meta <- suppressMessages(as_epidist_meta_model(
  sim_obs,
  estimates = np_meta_estimates
))

test_that("the meta model takes the nonparametric family", {
  family <- epidist_family(np_meta, family = nonparametric())
  expect_identical(family$name, "meta_discretehazard_rw")
  expect_identical(max(family$np$boundaries), 30)
  code <- suppressMessages(epidist(
    np_meta,
    family = nonparametric(),
    fn = brms::make_stancode
  ))
  code <- as.character(code)
  expect_match(code, "y | 27,", fixed = TRUE)
  expect_match(code, "if (27 == 27 || 27 == 28) {", fixed = TRUE)
  skip_on_cran()
  expect_no_error(rstan::stanc(model_code = code))
})

test_that("the meta model sets default boundaries from summaries alone", {
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = np_meta_estimates)
  )
  family <- epidist_family(meta, family = nonparametric())
  expect_identical(family$np$boundaries, as.numeric(seq(-1, 30)))
})

test_that("the meta model rejects continuous summaries it cannot imply", {
  # Study B of prep_meta_obs reports a quantile of the continuous delay
  expect_error(
    epidist_family(prep_meta_obs, family = nonparametric()),
    "\"B\""
  )
  left <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "D", type = "mean", value = 6, n = 50,
    relative_obs_time = Inf, trunc_adjusted = TRUE, cens_adjusted = 1,
    delay_min = 1, stringsAsFactors = FALSE
  )))
  expect_error(
    epidist_family(
      suppressMessages(as_epidist_meta_model(estimates = left)),
      family = nonparametric(-1:20)
    ),
    "\"D\""
  )
  truncated <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "E", type = "mean", value = 6, n = 50,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 1,
    stringsAsFactors = FALSE
  )))
  expect_error(
    epidist_family(
      suppressMessages(as_epidist_meta_model(estimates = truncated)),
      family = nonparametric(-1:20)
    ),
    "\"E\""
  )
})

test_that("the meta model moments of the bins match primarycensored", {
  args <- list(boundaries = c(-1, 0, 1, 3, 6), hazards = c(0.2, 0.3, 0.5, 1))
  moments <- .meta_continuous_moments("pdiscretehazard", args)
  mass <- primarycensored::hazards_to_pmf(args$hazards)
  edges <- c(0, 1, 3, 6)
  delay_mean <- sum(mass * edges)
  variance <- sum(mass * (edges - delay_mean)^2)
  expect_equal(unname(moments[1]), delay_mean, tolerance = 1e-12)
  expect_equal(unname(moments[2]), sqrt(variance), tolerance = 1e-12)
  set.seed(2)
  draws <- primarycensored::rdiscretehazard(
    1e5,
    boundaries = args$boundaries, hazards = args$hazards
  )
  expect_equal(unname(moments[1]), mean(draws), tolerance = 0.01)
})

test_that("the uniform primary censored density leaves out mass at zero", {
  args <- list(boundaries = c(-1, 0, 1, 3, 6), hazards = c(0.2, 0.3, 0.5, 1))
  # Over (y - 1, y] with y below the window the only mass is at zero
  expect_equal(
    .meta_uniform_pcens_density(0.5, "pdiscretehazard", args, 1), 0.2
  )
  expect_equal(
    .meta_uniform_pcens_density(1.5, "pdiscretehazard", args, 1),
    primarycensored::hazards_to_pmf(args$hazards)[2],
    tolerance = 1e-12
  )
})

test_that("the meta model has no density for the nonparametric family", {
  expect_error(.meta_ddist("pdiscretehazard"), "no density")
})

# A prepared predictions object in the form brms passes to the log
# likelihood, prediction and expectation functions of a custom family, with
# two draws of the non-parametric parameters for three observations.
np_prep <- function(boundaries = -1:4, hazard_model = "rw") {
  n_bins <- length(boundaries) - 1
  eps <- .np_eps_names(n_bins, hazard_model)
  draws <- c(
    list(mu = c(-1, -0.5), hsigma = c(0.5, 0.8)),
    stats::setNames(
      lapply(seq_along(eps), function(j) c(0.3, -0.2) * j),
      eps
    )
  )
  dpars <- lapply(draws, matrix, nrow = 2, ncol = 3)
  prep <- structure(
    list(
      ndraws = 2, nobs = 3, dpars = dpars,
      data = list(
        Y = c(0, 1, 3), vreal1 = c(Inf, 10, 6), vreal2 = c(1, 1, 1),
        vreal3 = c(1, 1, 1), vreal4 = c(1, 2, 4), vreal5 = c(0, 0, 0)
      )
    ),
    class = "brmsprep"
  )
  np <- list(boundaries = as.numeric(boundaries), hazard_model = hazard_model)
  return(list(prep = prep, draws = draws, np = np))
}

test_that(".np_dist_args() gives the hazards of each draw", {
  for (hazard_model in c("rw", "re")) {
    setup <- np_prep(hazard_model = hazard_model)
    args <- .np_dist_args(setup$prep, 2, setup$np)
    expect_length(args, 2)
    hazards <- .np_hazards(setup$draws, hazard_model)
    for (draw in 1:2) {
      expect_identical(args[[draw]]$boundaries, setup$np$boundaries)
      expect_identical(args[[draw]]$hazards, hazards[draw, ])
    }
  }
})

test_that("the nonparametric log likelihood matches primarycensored", {
  setup <- np_prep()
  family <- nonparametric(-1:4)
  log_lik <- epidist_gen_log_lik(family)
  hazards <- .np_hazards(setup$draws, "rw")
  for (i in 1:3) {
    data <- setup$prep$data
    expected <- vapply(1:2, function(draw) {
      return(primarycensored::dpcens(
        data$Y[i], primarycensored::pdiscretehazard,
        pwindow = data$vreal2[i], swindow = data$vreal3[i],
        L = data$vreal5[i], D = data$vreal1[i], log = TRUE,
        boundaries = family$np$boundaries, hazards = hazards[draw, ]
      ))
    }, numeric(1))
    expect_equal(log_lik(i, setup$prep), expected, tolerance = 1e-10)
  }
})

test_that("nonparametric posterior draws lie on the bin edges", {
  setup <- np_prep()
  rdist <- .np_rdist(setup$np)
  withr::local_seed(1)
  # More delays than draws recycles the draws
  delays <- rdist(1000, 1, setup$prep)
  expect_length(delays, 1000)
  expect_true(all(delays %in% setup$np$boundaries[-1]))
  mass <- .np_pmf(setup$draws, setup$np$boundaries, "rw")
  expect_equal(
    mean(delays), mean(mass %*% setup$np$boundaries[-1]),
    tolerance = 0.1
  )
  predict <- epidist_gen_posterior_predict(nonparametric(-1:4))
  withr::local_seed(2)
  predicted <- predict(2, setup$prep)
  expect_identical(dim(predicted), c(2L, 1L))
  # Observation 2 is truncated at a delay of 10, which the bins never reach,
  # and every delay is a whole number of days after daily censoring
  expect_identical(predicted, round(predicted))
})

test_that("the nonparametric expected delay is the mean of the bins", {
  setup <- np_prep()
  epred <- epidist_gen_posterior_epred(nonparametric(-1:4))(setup$prep)
  expect_identical(dim(epred), c(2L, 3L))
  mass <- .np_pmf(setup$draws, setup$np$boundaries, "rw")
  expected <- as.vector(mass %*% setup$np$boundaries[-1])
  # The parameters are the same for every observation
  for (i in 1:3) {
    expect_equal(epred[, i], expected, tolerance = 1e-12)
  }
})
