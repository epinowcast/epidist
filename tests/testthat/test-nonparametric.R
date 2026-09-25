test_that("nonparametric() builds a spline hazard family by default", {
  family <- nonparametric(boundaries = -1:20)
  expect_s3_class(family, "brmsfamily")
  expect_identical(family$family, "nonparametric")
  expect_identical(family$np$boundaries, as.numeric(-1:20))
  expect_identical(
    deparse(family$np$formula), "~s(delay, k = 10)"
  )
  # A thin plate spline of ten basis functions less the intercept is one
  # unpenalised column and eight penalised ones with a shared sd
  expect_identical(
    family$dpars,
    c("mu", "h1b", "h1sd", paste0("h", 1:8, "z"))
  )
  expect_identical(family$link, "identity")
  expect_identical(family$other_links, c("identity", "log", rep("identity", 8)))
  expect_identical(family$other_bounds[[2]]$lb, "0")
  expect_identical(dim(family$np$basis), c(20L, 9L))
})

test_that("the default spline has no more basis functions than bins", {
  family <- nonparametric(boundaries = -1:4)
  expect_identical(deparse(family$np$formula), "~s(delay, k = 4)")
  family <- nonparametric(boundaries = -1:2)
  expect_identical(deparse(family$np$formula), "~s(bin, bs = \"re\")")
  expect_identical(family$dpars, c("mu", "h1sd", "h1z", "h2z"))
})

test_that("nonparametric() takes brms terms over the bins", {
  family <- nonparametric(~ (1 | bin) + delay, boundaries = -1:6)
  expect_identical(
    deparse(family$np$formula), "~s(bin, bs = \"re\") + delay"
  )
  expect_identical(
    family$np$coefficients$term,
    c("delay", rep("s(bin)", 6))
  )
  expect_identical(
    family$dpars,
    c("mu", "h1b", "h1sd", paste0("h", 1:6, "z"))
  )
  linear <- nonparametric(~ delay + I(delay^2), boundaries = -1:6)
  expect_identical(linear$dpars, c("mu", "h1b", "h2b"))
  expect_true(all(is.na(linear$np$coefficients$sd)))
  constant <- nonparametric(~1, boundaries = -1:6)
  expect_identical(constant$dpars, "mu")
  expect_identical(dim(constant$np$basis), c(6L, 0L))
  pspline <- nonparametric(
    ~ s(delay, bs = "ps", k = 5),
    boundaries = c(-1:5, 7, 10)
  )
  expect_identical(pspline$np$coefficients$term[1], "s(delay)")
})

test_that("the hazard basis is centred and scaled over the bins", {
  family <- nonparametric(~ delay + s(bin, bs = "re"), boundaries = -1:8)
  basis <- family$np$basis
  expect_equal(colMeans(basis), rep(0, ncol(basis)), tolerance = 1e-12)
  expect_equal(mean(basis[, 1]^2), 1, tolerance = 1e-12)
  expect_equal(mean(rowSums(basis[, -1]^2)), 1, tolerance = 1e-12)
})

test_that("nonparametric() leaves the parameters to the data by default", {
  family <- nonparametric()
  expect_null(family$np$boundaries)
  expect_null(family$np$formula)
  expect_identical(family$dpars, "mu")
})

test_that("nonparametric() rejects boundaries it cannot use", {
  expect_error(nonparametric(boundaries = c(0, 2, 1, 3)), "increasing")
  expect_error(nonparametric(boundaries = c(-1, 0, 1)), "at least")
  expect_error(nonparametric(boundaries = c(-1, 0, 1, NA)))
})

test_that("nonparametric() rejects formulas it cannot build", {
  expect_error(nonparametric(y ~ delay), "one sided")
  expect_error(nonparametric(~age_group), "age_group")
  expect_error(nonparametric(~ (delay | bin)), "random")
  expect_error(
    nonparametric(~ delay + bin, boundaries = -1:6), "not identified"
  )
})

test_that("the hazard family maps to the primarycensored hazard likelihood", {
  # primarycensored gives the alias "nonparametric" to the direct PMF step,
  # dist_id 26, so the family is looked up by its hazard name.
  family <- epidist_family(prep_marginal_obs, family = nonparametric())
  code <- .family_functions_stanvar(
    file.path("marginal_model", "functions.stan"), family, "marginal_"
  )[[1]]$scode
  expect_match(code, "y | 27,", fixed = TRUE)
})

np_family <- nonparametric(~ delay + (1 | bin), boundaries = -1:4)
np_draws <- data.frame(
  mu = c(-1, -0.5), h1b = c(0.3, -0.2), h1sd = c(0.5, 0.8),
  h1z = c(0.1, -1), h2z = c(-0.5, 0.4), h3z = c(0.2, 0.1),
  h4z = c(1, -0.3)
)

test_that(".np_hazards() is mu plus the basis times the coefficients", {
  hazards <- .np_hazards(np_draws, np_family$np)
  expect_identical(dim(hazards), c(2L, 5L))
  expect_identical(hazards[, 5], c(1, 1))
  basis <- np_family$np$basis
  for (d in 1:2) {
    coefs <- c(
      np_draws$h1b[d],
      np_draws$h1sd[d] * unlist(np_draws[d, paste0("h", 1:4, "z")])
    )
    expect_equal(
      hazards[d, 1:4],
      stats::plogis(np_draws$mu[d] + as.vector(basis %*% coefs)),
      tolerance = 1e-12
    )
  }
})

test_that(".np_pmf() matches primarycensored", {
  pmf <- .np_pmf(np_draws, np_family$np)
  hazards <- .np_hazards(np_draws, np_family$np)
  for (d in 1:2) {
    expect_equal(
      pmf[d, ], primarycensored::hazards_to_pmf(hazards[d, ]),
      tolerance = 1e-12
    )
  }
})

test_that("epidist_family() sets default boundaries from the marginal data", {
  family <- epidist_family(prep_marginal_obs, family = nonparametric())
  expect_s3_class(family, "customfamily")
  expect_identical(family$name, "marginal_nonparametric")
  longest <- max(prep_marginal_obs$delay_upr)
  expect_identical(family$np$boundaries, as.numeric(seq(-1, longest)))
  expect_identical(family$dpars[1:3], c("mu", "h1b", "h1sd"))
  expect_identical(
    family$param,
    paste0(
      "epidist_np_params(epidist_np_boundaries(), mu, epidist_np_basis(), ",
      "[h1b, ", toString(paste0("h1sd * h", 1:8, "z")), "]')"
    )
  )
})

test_that("epidist_family() keeps at least three free bins by default", {
  data <- prep_marginal_obs[prep_marginal_obs$delay_upr <= 2, ]
  family <- epidist_family(data, family = nonparametric())
  expect_identical(family$np$boundaries, as.numeric(-1:3))
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
    epidist_family(
      prep_marginal_obs,
      family = nonparametric(boundaries = -1:3)
    ),
    "longest observed delay"
  )
})

test_that("the nonparametric Stan functions hold the bins exactly", {
  family <- epidist_family(
    prep_marginal_obs,
    family = nonparametric(~delay, boundaries = c(-1, 0.5, 2, 30))
  )
  code <- .np_stanvars(family)[[1]]$scode
  expect_match(code, "return {-1.0, 0.5, 2.0, 30.0};", fixed = TRUE)
  basis <- sprintf("%.17g", family$np$basis)
  for (value in basis) {
    expect_match(code, paste0("[", value, "]"), fixed = TRUE)
  }
  expect_identical(.np_stan_reals(c(1, 0.1, 1e-20)), c(
    "1.0", "0.10000000000000001", "9.9999999999999995e-21"
  ))
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
  expect_match(code, "matrix epidist_np_basis() {", fixed = TRUE)
  expect_match(
    code,
    "marginal_nonparametric_lpmf(Y[n] | mu[n], h1b[n], h1sd[n], h1z[n]",
    fixed = TRUE
  )
  skip_on_cran()
  expect_no_error(rstan::stanc(model_code = code))
})

test_that("a constant hazard compiles with an empty basis", {
  code <- suppressMessages(epidist(
    prep_marginal_obs,
    family = nonparametric(~1, boundaries = -1:25),
    fn = brms::make_stancode
  ))
  code <- as.character(code)
  expect_match(code, "rep_matrix(0, 25, 0)", fixed = TRUE)
  expect_match(code, "rep_vector(0, 0))", fixed = TRUE)
  skip_on_cran()
  expect_no_error(rstan::stanc(model_code = code))
})

test_that("the nonparametric family sets its default priors", {
  family <- epidist_family(
    prep_marginal_obs,
    nonparametric(~ delay + (1 | bin), boundaries = -1:25)
  )
  formula <- epidist_formula(prep_marginal_obs, family, formula = mu ~ 1)
  prior <- suppressMessages(
    epidist_prior(prep_marginal_obs, family, formula, prior = NULL)
  )
  intercept <- prior[prior$class == "Intercept", ]
  # 26 bins, so the hazard of the first is 1 / 26 when all are equally likely
  expect_identical(
    intercept$prior[!nzchar(intercept$dpar)], "normal(-3.22, 1.5)"
  )
  expect_identical(intercept$prior[intercept$dpar == "h1b"], "normal(0, 2)")
  expect_identical(intercept$prior[intercept$dpar == "h1sd"], "normal(0, 1)")
  z <- intercept$prior[grepl("^h[0-9]+z$", intercept$dpar)]
  expect_length(z, 25)
  expect_true(all(z == "std_normal()"))
})

test_that("a hazard coefficient takes a formula for a non-proportional effect", { # nolint: line_length_linter.
  data <- prep_marginal_obs
  data$group <- rep_len(c("a", "b"), nrow(data))
  family <- nonparametric(~delay, boundaries = -1:25)
  code <- suppressMessages(epidist(
    data,
    formula = bf(mu ~ group, h1b ~ group),
    family = family,
    fn = brms::make_stancode
  ))
  expect_match(as.character(code), "b_h1b", fixed = TRUE)
})

test_that("add_summaries() gives the moments and quantiles of the bins", {
  out <- add_summaries(
    np_draws,
    family = np_family, probs = c(0.1, 0.5, 0.99)
  )
  pmf <- .np_pmf(np_draws, np_family$np)
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
  analytic <- add_summaries(np_draws, family = np_family)
  withr::local_seed(1)
  sampled <- add_summaries(
    np_draws,
    family = np_family, method = "sample", nsim = 20000
  )
  expect_equal(sampled$mean, analytic$mean, tolerance = 0.02)
  expect_equal(sampled$sd, analytic$sd, tolerance = 0.03)
})

test_that("the nonparametric density is the histogram of the bins", {
  summaries <- .np_delay_summaries(np_family$np)
  pmf <- .np_pmf(np_draws, np_family$np)
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
  expect_identical(family$name, "meta_nonparametric")
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
      family = nonparametric(boundaries = -1:20)
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
      family = nonparametric(boundaries = -1:20)
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

test_that("the nonparametric family has no density or quantile function", {
  expect_error(.ddist("pdiscretehazard"), "no density")
  expect_error(.qdist("pdiscretehazard"), "no density")
  expect_identical(.pdist("pdiscretehazard"), primarycensored::pdiscretehazard)
})

# A prepared predictions object in the form brms passes to the log
# likelihood, prediction and expectation functions of a custom family, with
# the two draws of np_draws for three observations.
np_prep <- function() {
  dpars <- lapply(as.list(np_draws), matrix, nrow = 2, ncol = 3)
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
  return(list(prep = prep, draws = np_draws, np = np_family$np))
}

test_that(".np_dist_args() gives the hazards of each draw", {
  setup <- np_prep()
  args <- .np_dist_args(setup$prep, 2, setup$np)
  expect_length(args, 2)
  hazards <- .np_hazards(setup$draws, setup$np)
  for (draw in 1:2) {
    expect_identical(args[[draw]]$boundaries, setup$np$boundaries)
    expect_identical(args[[draw]]$hazards, hazards[draw, ])
  }
})

test_that("the nonparametric log likelihood matches primarycensored", {
  setup <- np_prep()
  log_lik <- epidist_gen_log_lik(np_family)
  hazards <- .np_hazards(setup$draws, setup$np)
  for (i in 1:3) {
    data <- setup$prep$data
    expected <- vapply(1:2, function(draw) {
      return(primarycensored::dpcens(
        data$Y[i], primarycensored::pdiscretehazard,
        pwindow = data$vreal2[i], swindow = data$vreal3[i],
        L = data$vreal5[i], D = data$vreal1[i], log = TRUE,
        boundaries = setup$np$boundaries, hazards = hazards[draw, ]
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
  mass <- .np_pmf(setup$draws, setup$np)
  expect_equal(
    mean(delays), mean(mass %*% setup$np$boundaries[-1]),
    tolerance = 0.1
  )
  predict <- epidist_gen_posterior_predict(np_family)
  withr::local_seed(2)
  predicted <- predict(2, setup$prep)
  expect_identical(dim(predicted), c(2L, 1L))
  # Every delay is a whole number of days after daily censoring
  expect_identical(predicted, round(predicted))
})

test_that("the nonparametric expected delay is the mean of the bins", {
  setup <- np_prep()
  epred <- epidist_gen_posterior_epred(np_family)(setup$prep)
  expect_identical(dim(epred), c(2L, 3L))
  mass <- .np_pmf(setup$draws, setup$np)
  expected <- as.vector(mass %*% setup$np$boundaries[-1])
  # The parameters are the same for every observation
  for (i in 1:3) {
    expect_equal(epred[, i], expected, tolerance = 1e-12)
  }
})
