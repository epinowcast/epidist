test_that("add_summaries adds the analytic mean and sd for a lognormal", {
  draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
  out <- add_summaries(draws, family = "lognormal")
  expect_named(out, c("mu", "sigma", "mean", "sd"))
  expect_identical(out$mean, exp(draws$mu + draws$sigma^2 / 2))
  expect_identical(out$sd, out$mean * sqrt(exp(draws$sigma^2) - 1))
})

test_that("add_summaries adds the analytic mean and sd for a gamma", {
  draws <- data.frame(mu = c(6, 8), shape = c(2, 3))
  out <- add_summaries(draws, family = "gamma")
  expect_named(out, c("mu", "shape", "mean", "sd"))
  expect_identical(out$mean, draws$mu)
  expect_identical(out$sd, draws$mu / sqrt(draws$shape))
})

test_that("add_summaries adds the analytic mean and sd for a weibull", {
  draws <- data.frame(mu = c(6, 8), shape = c(2, 3))
  out <- add_summaries(draws, family = "weibull")
  expect_named(out, c("mu", "shape", "mean", "sd"))
  expect_identical(out$mean, draws$mu)
  expect_true(all(out$sd > 0))
})

test_that("add_summaries adds quantiles named as in posterior", {
  draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
  out <- add_summaries(draws, family = "lognormal", probs = c(0.025, 0.5, 0.9))
  expect_named(out, c("mu", "sigma", "mean", "sd", "q2.5", "q50", "q90"))
  expect_identical(out$q50, stats::qlnorm(0.5, draws$mu, draws$sigma))
  expect_identical(out$q90, stats::qlnorm(0.9, draws$mu, draws$sigma))
})

test_that("add_summaries by simulation agrees with the analytic solution", {
  set.seed(1)
  draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
  analytic <- add_summaries(draws, family = "lognormal", probs = 0.5)
  sampled <- add_summaries(
    draws,
    family = "lognormal",
    probs = 0.5,
    method = "sample",
    nsim = 20000
  )
  expect_named(sampled, names(analytic))
  expect_equal(sampled$mean, analytic$mean, tolerance = 0.05)
  expect_equal(sampled$sd, analytic$sd, tolerance = 0.1)
  expect_equal(sampled$q50, analytic$q50, tolerance = 0.05)
})

test_that("add_summaries simulates for a family with no analytic solution", {
  set.seed(1)
  draws <- data.frame(mu = c(2, 4))
  out <- add_summaries(draws, family = "exponential", nsim = 20000)
  expect_named(out, c("mu", "mean", "sd"))
  expect_equal(out$mean, draws$mu, tolerance = 0.05)
  expect_equal(out$sd, draws$mu, tolerance = 0.05)
})

test_that("add_summaries errors when asked for an unavailable analytic solution", { # nolint: line_length_linter.
  draws <- data.frame(mu = c(2, 4))
  expect_error(
    add_summaries(draws, family = "exponential", method = "analytic"),
    "No analytic delay summaries"
  )
})

test_that("add_summaries errors when the family cannot be worked out", {
  expect_error(
    add_summaries(data.frame(mu = 1, sigma = 1)),
    "Could not work out the delay distribution family"
  )
})

test_that("add_summaries errors when a distributional parameter is missing", {
  expect_error(
    add_summaries(data.frame(mu = 1), family = "lognormal", method = "sample"),
    "missing distributional parameters"
  )
})

test_that("add_summaries errors on its input arguments", {
  draws <- data.frame(mu = 1, sigma = 1)
  expect_error(
    add_summaries("not a data.frame", family = "lognormal"),
    "data.frame"
  )
  expect_error(
    add_summaries(draws, family = "lognormal", probs = c(-0.1)),
    "not >= 0"
  )
  expect_error(
    add_summaries(draws, family = "lognormal", probs = 1.5),
    "not <= 1"
  )
  expect_error(
    add_summaries(draws, family = "lognormal", nsim = 0),
    "not >= 1"
  )
  expect_error(
    add_summaries(draws, family = "lognormal", method = "bogus"),
    "auto.*analytic.*sample"
  )
})

test_that("add_summaries errors for an analytic family missing parameters", {
  # lognormal has an analytic solution, so this exercises the assert path
  # inside the analytic branch rather than the no-analytic-solution error.
  expect_error(
    add_summaries(
      data.frame(mu = 1),
      family = "lognormal",
      method = "analytic"
    ),
    "missing distributional parameters"
  )
})

test_that("add_summaries returns an epidist_delay_draws object", {
  draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
  out <- add_summaries(draws, family = "lognormal")
  expect_s3_class(out, "epidist_delay_draws")
  expect_s3_class(out, "data.frame")
  expect_identical(attr(out, "epidist_family")$name, "lognormal")
  # A grouped tibble keeps its grouping and what the class records
  grouped <- dplyr::group_by(tibble::as_tibble(draws), mu)
  grouped <- .new_delay_draws(grouped, .delay_family(brms::lognormal()), "mu")
  out <- add_summaries(grouped)
  expect_s3_class(out, "epidist_delay_draws")
  expect_s3_class(out, "grouped_df")
  expect_identical(dplyr::group_vars(out), "mu")
  expect_identical(attr(out, "epidist_vars"), "mu")
  # dplyr verbs and subsetting still work on the class
  expect_identical(dplyr::filter(out, mu > 1.9)$mu, 2.0)
  expect_s3_class(dplyr::mutate(out, x = 1), "grouped_df")
  expect_identical(nrow(dplyr::ungroup(out)), 2L)
  expect_identical(nrow(out[1, ]), 1L)
  expect_named(
    dplyr::summarise(out, mean = mean(mean), .groups = "drop"),
    c("mu", "mean")
  )
})

test_that(".analytic_delay_summaries gives the quantile and density", {
  d <- list(mu = c(6, 8), shape = c(2, 3), sigma = c(0.5, 0.4))
  lognormal <- .analytic_delay_summaries("lognormal")
  expect_identical(lognormal$dpars, c("mu", "sigma"))
  expect_identical(
    lognormal$quantile(d, 0.5),
    stats::qlnorm(0.5, meanlog = d$mu, sdlog = d$sigma)
  )
  expect_identical(
    lognormal$density(d, 3),
    stats::dlnorm(3, meanlog = d$mu, sdlog = d$sigma)
  )
  gamma <- .analytic_delay_summaries("gamma")
  expect_identical(gamma$dpars, c("mu", "shape"))
  expect_identical(
    gamma$quantile(d, 0.5),
    stats::qgamma(0.5, shape = d$shape, rate = d$shape / d$mu)
  )
  expect_identical(
    gamma$density(d, 3),
    stats::dgamma(3, shape = d$shape, rate = d$shape / d$mu)
  )
  weibull <- .analytic_delay_summaries("weibull")
  expect_identical(weibull$dpars, c("mu", "shape"))
  scale <- d$mu / gamma(1 + 1 / d$shape)
  expect_identical(
    weibull$quantile(d, 0.5),
    stats::qweibull(0.5, shape = d$shape, scale = scale)
  )
  expect_identical(
    weibull$density(d, 3),
    stats::dweibull(3, shape = d$shape, scale = scale)
  )
  # The density of each family integrates to one
  for (family in list(lognormal, gamma, weibull)) {
    total <- stats::integrate(
      function(x) family$density(list(mu = 6, shape = 2, sigma = 0.5), x),
      0,
      Inf
    )$value
    expect_equal(total, 1, tolerance = 1e-6)
  }
  expect_null(.analytic_delay_summaries("exponential"))
})

test_that("add_summaries accepts a stats family object", {
  draws <- data.frame(mu = c(6, 8), shape = c(2, 3))
  out <- add_summaries(draws, family = Gamma())
  expect_named(out, c("mu", "shape", "mean", "sd"))
  expect_identical(out$mean, draws$mu)
})

test_that("add_summaries simulates in chunks without changing the answer", {
  set.seed(1)
  draws <- data.frame(mu = rep(1.8, 3), sigma = rep(0.5, 3))
  # `nsim` above the chunk size means each row is simulated in its own chunk
  out <- add_summaries(
    draws,
    family = "lognormal",
    method = "sample",
    nsim = 2e6
  )
  expect_equal(out$mean, rep(exp(1.8 + 0.25 / 2), 3), tolerance = 0.01)
})

test_that("add_summaries errors when it cannot simulate from a family", {
  # mu outside the support of the binomial forces the posterior prediction
  # to fail, exercising the simulation error path.
  expect_error(
    add_summaries(
      data.frame(mu = 2),
      family = "binomial",
      method = "sample",
      nsim = 10
    ),
    "Could not simulate delays"
  )
})

test_that("the delay family drops the epidist model prefix", {
  expect_identical(
    .delay_family(list(name = "latent_lognormal"))$name,
    "lognormal"
  )
  expect_identical(.delay_family(list(name = "marginal_gamma"))$name, "gamma")
  expect_identical(.delay_family(brms::lognormal())$name, "lognormal")
  expect_identical(.delay_family(brms::lognormal())$dpars, c("mu", "sigma"))
})

test_that("epidist_strata returns unique combinations of the predictors", {
  data <- data.frame(
    y = c(1, 2, 3),
    x = c("a", "b", "a"),
    z = 1:3,
    stringsAsFactors = FALSE
  )
  object <- list(formula = brms::bf(y ~ x, sigma ~ 1), data = data)
  strata <- epidist_strata(object)
  expect_s3_class(strata, "tbl_df")
  expect_identical(nrow(strata), 2L)
  expect_identical(names(strata)[1], "x")
  expect_identical(strata$x, c("a", "b"))
  # Columns not in the formula are kept from the first row of each combination
  expect_identical(strata$z, c(1L, 2L))
})

test_that("epidist_strata returns one row for a model with no predictors", {
  data <- data.frame(
    y = c(1, 2, 3),
    x = c("a", "b", "a"),
    z = 1:3,
    stringsAsFactors = FALSE
  )
  object <- list(formula = brms::bf(y ~ 1), data = data)
  expect_identical(nrow(epidist_strata(object)), 1L)
})

test_that("epidist_strata uses the variables it is given", {
  data <- data.frame(
    y = c(1, 2, 3),
    x = c("a", "b", "a"),
    z = 1:3,
    stringsAsFactors = FALSE
  )
  object <- list(formula = brms::bf(y ~ 1), data = data)
  expect_identical(nrow(epidist_strata(object, vars = "z")), 3L)
  expect_error(epidist_strata(object, vars = "missing"), "missing")
})

test_that("epidist_strata errors when the object has no fitted data", {
  object <- list(formula = brms::bf(y ~ x), data = NULL)
  expect_error(epidist_strata(object), "does not contain the data")
})

test_that("delay_parameter_draws works with NULL newdata and the latent and marginal lognormal model", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()

  test_draws <- function(fit, expected_rows = nrow(prep_obs)) {
    draws <- delay_parameter_draws(fit)
    expect_s3_class(draws, "epidist_delay_draws")
    expect_s3_class(draws, "grouped_df")
    expect_identical(attr(draws, "epidist_family")$name, "lognormal")
    expect_true(
      all(
        c(".row", ".chain", ".iteration", ".draw", "mu", "sigma") %in%
          names(draws)
      )
    )
    expect_identical(utils::tail(names(draws), 2), c("mu", "sigma"))
    expect_identical(utils::tail(dplyr::group_vars(draws), 1), ".row")
    expect_length(unique(draws$.row), expected_rows)
    expect_length(unique(draws$.draw), summary(fit)$total_ndraws)
    expect_setequal(unique(draws$.chain), c(1, 2))
    return(invisible(NULL))
  }

  test_draws(fit)
  test_draws(fit_marginal, expected_rows = 144)
})

test_that("delay_summary_draws matches the steps it wraps", {
  skip_on_cran()
  skip_if_no_fits()

  expect_identical(
    delay_summary_draws(fit_marginal_sex, probs = c(0.05, 0.95)),
    add_summaries(
      delay_parameter_draws(
        fit_marginal_sex,
        newdata = epidist_strata(fit_marginal_sex)
      ),
      probs = c(0.05, 0.95)
    )
  )
})

test_that("delay_summary_draws strata by default and keeps the grouping", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- delay_summary_draws(fit_marginal_sex)
  expect_s3_class(draws, "epidist_delay_draws")
  expect_s3_class(draws, "grouped_df")
  expect_true(all(c("mean", "sd", "mu", "sigma") %in% names(draws)))
  expect_identical(utils::tail(dplyr::group_vars(draws), 1), ".row")
  # One stratum per unique combination of the predictors, not one per row of
  # the data the model was fitted to.
  expect_length(
    unique(draws$.row), nrow(epidist_strata(fit_marginal_sex))
  )
  expect_lt(
    nrow(epidist_strata(fit_marginal_sex)), nrow(fit_marginal_sex$data)
  )
})

test_that("delay_summary_draws passes vars, probs and dots on", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- delay_summary_draws(
    fit_marginal_sex,
    vars = "sex", probs = c(0.25, 0.75), ndraws = 50
  )
  expect_true(all(c("q25", "q75") %in% names(draws)))
  expect_length(unique(draws$.draw), 50)
  expect_length(unique(draws$.row), 2)
})

test_that("delay_parameter_draws matches add_delay_parameter_draws", {
  skip_on_cran()
  skip_if_no_fits()

  strata <- epidist_strata(fit)
  expect_identical(
    add_delay_parameter_draws(strata, fit),
    delay_parameter_draws(fit, newdata = strata)
  )
})

test_that("delay_parameter_draws keeps the columns of newdata", {
  skip_on_cran()
  skip_if_no_fits()

  strata <- epidist_strata(fit_sex)
  draws <- delay_parameter_draws(fit_sex, newdata = strata)
  expect_true(all(names(strata) %in% names(draws)))
  expect_identical(attr(draws, "epidist_vars"), "sex")
  expect_identical(
    nrow(draws),
    as.integer(nrow(strata) * summary(fit_sex)$total_ndraws)
  )
  expect_setequal(draws$sex, strata$sex)
})

test_that("delay_parameter_draws subsets draws and reports no chain", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- delay_parameter_draws(fit, ndraws = 10)
  expect_length(unique(draws$.draw), 10)
  expect_true(all(is.na(draws$.chain)))
  expect_true(all(is.na(draws$.iteration)))
})

test_that("epidist_strata reduces a fitted model to its unique strata", {
  skip_on_cran()
  skip_if_no_fits()

  expect_identical(nrow(epidist_strata(fit)), 1L)
  strata_sex <- epidist_strata(fit_sex)
  expect_identical(nrow(strata_sex), 2L)
  expect_setequal(strata_sex$sex, c(0, 1))
  expect_identical(names(strata_sex)[1], "sex")
})

test_that("delay_parameter_draws by strata recovers the underlying parameters", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()

  test_sex_draws <- function(fit) {
    draws <- fit |>
      epidist_strata() |>
      add_delay_parameter_draws(fit) |>
      dplyr::group_by(sex) |>
      dplyr::summarise(mu = mean(mu), sigma = mean(sigma))

    expect_equal(
      as.numeric(draws[draws$sex == 0, c("mu", "sigma")]),
      c(meanlog_m, sdlog_m),
      tolerance = 0.1
    )
    expect_equal(
      as.numeric(draws[draws$sex == 1, c("mu", "sigma")]),
      c(meanlog_f, sdlog_f),
      tolerance = 0.1
    )
    return(invisible(NULL))
  }

  test_sex_draws(fit_sex)
  test_sex_draws(fit_marginal_sex)
})

test_that("add_summaries takes the family from a fit", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- fit |>
    epidist_strata() |>
    add_delay_parameter_draws(fit) |>
    dplyr::ungroup()
  expect_error(add_summaries(draws), "Could not work out")
  expect_true(all(add_summaries(draws, family = fit)$mean > 0))
  expect_true(all(add_summaries(draws, family = fit$family)$mean > 0))
})

test_that("add_summaries uses the family recorded by delay_parameter_draws", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- fit |>
    epidist_strata() |>
    add_delay_parameter_draws(fit) |>
    add_summaries(probs = c(0.05, 0.95))
  expect_s3_class(draws, "grouped_df")
  expect_identical(utils::tail(names(draws), 4), c("mean", "sd", "q5", "q95"))
  expect_true(all(draws$mean > 0))
  expect_true(all(draws$sd > 0))
  expect_true(all(draws$q5 < draws$q95))
  expect_equal(
    draws$mean,
    exp(draws$mu + draws$sigma^2 / 2),
    ignore_attr = TRUE
  )
})

test_that("add_summaries by simulation agrees with the analytic solution for a fitted model", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()

  set.seed(1)
  draws <- fit |>
    epidist_strata() |>
    add_delay_parameter_draws(fit)
  analytic <- add_summaries(draws)
  sampled <- add_summaries(draws, method = "sample", nsim = 5000)
  expect_equal(mean(sampled$mean), mean(analytic$mean), tolerance = 0.05)
  expect_equal(mean(sampled$sd), mean(analytic$sd), tolerance = 0.1)
})

test_that("add_summaries works for the gamma and weibull models", {
  skip_on_cran()
  skip_if_no_fits()

  gamma_draws <- fit_gamma |>
    epidist_strata() |>
    add_delay_parameter_draws(fit_gamma) |>
    add_summaries()
  expect_identical(utils::tail(names(gamma_draws), 2), c("mean", "sd"))
  expect_equal(mean(gamma_draws$mean), mu, tolerance = 0.2)

  weibull_draws <- fit_marginal_weibull |>
    epidist_strata() |>
    add_delay_parameter_draws(fit_marginal_weibull) |>
    add_summaries()
  expect_identical(utils::tail(names(weibull_draws), 2), c("mean", "sd"))
  expect_true(all(weibull_draws$sd > 0))
})

test_that("delay_parameter_draws works with the naive model", {
  skip_on_cran()
  skip_if_no_fits()

  draws <- delay_parameter_draws(fit_naive)
  expect_true(all(c("mu", "sigma") %in% names(draws)))
  expect_length(unique(draws$.draw), summary(fit_naive)$total_ndraws)
  summaries <- add_summaries(draws)
  expect_true(all(summaries$mean > 0))
})
