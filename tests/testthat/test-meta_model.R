# fmt: skip file
test_that("as_epidist_meta_model works with individual level data only", {
  expect_s3_class(prep_meta_individual, "data.frame")
  expect_s3_class(prep_meta_individual, "epidist_meta_model")
  expect_true(all(prep_meta_individual$obs_type == 1L))
  expect_identical(nrow(prep_meta_individual), nrow(sim_obs))
})

test_that("as_epidist_meta_model works with estimates data only", {
  expect_s3_class(prep_meta_estimates, "epidist_meta_model")
  expect_false(any(prep_meta_estimates$obs_type == 1L))
  expect_identical(nrow(prep_meta_estimates), nrow(sim_estimates))
})

test_that("as_epidist_meta_model works with a mix of individual and summary data", { # nolint: line_length_linter.
  expect_s3_class(prep_meta_obs, "epidist_meta_model")
  expect_identical(
    nrow(prep_meta_obs), nrow(sim_obs) + nrow(sim_estimates)
  )
  expect_identical(sum(prep_meta_obs$obs_type == 1L), nrow(sim_obs))
})

test_that("as_epidist_meta_model errors when no data is supplied", {
  expect_error(as_epidist_meta_model(), "at least one")
  expect_error(as_epidist_meta_model(NULL, NULL), "at least one")
})

test_that("as_epidist_meta_model errors when passed incorrect inputs", {
  expect_error(as_epidist_meta_model(list()))
  expect_error(as_epidist_meta_model(sim_obs, estimates = list()))
})

test_that("as_epidist_meta_model works with aggregate data", {
  meta_agg <- suppressMessages(as_epidist_meta_model(agg_sim_obs))
  expect_s3_class(meta_agg, "epidist_meta_model")
  expect_identical(sum(meta_agg$n), sum(agg_sim_obs$n))
})

test_that("as_epidist_meta_model uses the design slot layout", {
  expect_named(
    prep_meta_obs[, .meta_required_cols()], .meta_required_cols()
  )
  summary_rows <- prep_meta_obs[prep_meta_obs$obs_type != 1L, ]
  expect_identical(
    summary_rows$obs_type,
    c(2L, 3L, 2L, 4L, 4L)
  )
  expect_true(all(summary_rows$delay_lwr == 0L))
  expect_true(all(summary_rows$n == 1))
  expect_identical(summary_rows$delay_upr, sim_estimates$value)
  individual_rows <- prep_meta_obs[prep_meta_obs$obs_type == 1L, ]
  expect_true(all(individual_rows$study_n == 0L))
  expect_true(all(individual_rows$report_se == 0))
  expect_true(all(individual_rows$quantile_p == 0))
  expect_true(all(individual_rows$growth_rate == 0))
})

test_that("as_epidist_meta_model sets the grid cutoff from the truncation flag", { # nolint: line_length_linter.
  summary_rows <- prep_meta_obs[prep_meta_obs$obs_type != 1L, ]
  expected <- ifelse(
    sim_estimates$trunc_adjusted,
    sim_estimates$max_delay,
    sim_estimates$relative_obs_time
  )
  expect_identical(summary_rows$relative_obs_time, expected)
})

test_that("as_epidist_meta_model labels individual rows in the study column", {
  expect_true("study" %in% names(prep_meta_obs))
  individual_rows <- prep_meta_obs[prep_meta_obs$obs_type == 1L, ]
  expect_true(all(individual_rows$study == "individual"))
})

test_that("is_epidist_meta_model returns TRUE for correct input", {
  expect_true(is_epidist_meta_model(prep_meta_obs))
})

test_that("is_epidist_meta_model returns FALSE for incorrect input", {
  expect_false(is_epidist_meta_model(list()))
  expect_false(is_epidist_meta_model(sim_obs))
})

test_that("assert_epidist.epidist_meta_model does not error for correct input", { # nolint: line_length_linter.
  expect_no_error(assert_epidist(prep_meta_obs))
  expect_no_error(assert_epidist(prep_meta_estimates))
  expect_no_error(assert_epidist(prep_meta_individual))
})

test_that("assert_epidist.epidist_meta_model errors for incorrect input", {
  expect_error(assert_epidist(prep_meta_obs[, 1]))
  bad <- prep_meta_obs
  bad$obs_type[1] <- 7L
  expect_error(assert_epidist(bad))
})

test_that("epidist_family_model.epidist_meta_model returns a meta custom family", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_obs, family = lognormal())
  expect_identical(family$name, "meta_lognormal")
  expect_identical(family$dpars, c("mu", "sigma"))
  expect_identical(family$type, "int")
  expect_true(all(paste0("vint", 1:4, "[n]") %in% family$vars))
  expect_true(all(paste0("vreal", 1:7, "[n]") %in% family$vars))
})

test_that("epidist_formula_model.epidist_meta_model binds the required slots", {
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family, formula = bf(mu ~ 1)
  )
  form <- as_string_formula(formula$formula)
  expect_true(grepl("delay_lwr", form, fixed = TRUE))
  expect_true(grepl("weights(n)", form, fixed = TRUE))
  expect_true(grepl(
    "vint(obs_type, study_n, trunc_adjusted, cens_adjusted)", form,
    fixed = TRUE
  ))
  expect_true(grepl(
    paste0(
      "vreal(relative_obs_time, pwindow, swindow, delay_upr, report_se, ",
      "quantile_p, growth_rate)"
    ),
    form,
    fixed = TRUE
  ))
})

test_that("epidist_transform_data_model.epidist_meta_model aggregates only individual rows", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family, formula = bf(mu ~ 1)
  )
  trans <- suppressMessages(
    epidist_transform_data_model(prep_meta_obs, family, formula)
  )
  expect_s3_class(trans, "epidist_meta_model")
  expect_identical(
    sum(trans$obs_type != 1L), nrow(sim_estimates)
  )
  expect_lt(sum(trans$obs_type == 1L), nrow(sim_obs))
  expect_identical(sum(trans$n), sum(prep_meta_obs$n))
})

test_that("epidist_stancode.epidist_meta_model produces valid stanvars", {
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family, formula = bf(mu ~ 1)
  )
  stancode <- epidist_stancode(
    prep_meta_obs, family = family, formula = formula
  )
  expect_s3_class(stancode, "stanvars")
  scode <- stancode[[2]]$scode
  expect_true(grepl("meta_lognormal_lpmf", scode, fixed = TRUE))
  expect_true(grepl("meta_lognormal_grid_pmf", scode, fixed = TRUE))
  expect_true(grepl("meta_lognormal_sd_se", scode, fixed = TRUE))
  expect_true(
    grepl("meta_lognormal_pcens_trunc_moments", scode, fixed = TRUE)
  )
  expect_true(grepl("meta_lognormal_survival_moments", scode, fixed = TRUE))
  expect_false(grepl("dpars_A", scode, fixed = TRUE))
  expect_false(grepl("dpars_B", scode, fixed = TRUE))
  expect_false(grepl("dist_id", scode, fixed = TRUE))
  # The normal theory standard error for a reported sd is ruled out by the
  # design in favour of the kurtosis based asymptotic standard error.
  expect_false(grepl("2.0 * (study_n - 1)", scode, fixed = TRUE))
})

# Numerical checks of the implied biased summaries against direct Monte Carlo
# simulation of the naive estimators they are meant to describe.

test_that(".meta_implied_moments matches Monte Carlo naive daily discretisation", { # nolint: line_length_linter.
  set.seed(101)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 30
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  n_sim <- 2e5
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  obs <- floor(stime[stime <= floor(cutoff)])
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
})

test_that(".meta_implied_moments matches Monte Carlo naive discretisation with a wide secondary window", { # nolint: line_length_linter.
  set.seed(102)
  args <- list(shape = 2, scale = 2.5)
  cutoff <- 40
  moments <- .meta_implied_moments(
    "pgamma", args, cutoff = cutoff, pwindow = 2, swindow = 2,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  n_sim <- 2e5
  ptime <- runif(n_sim, 0, 2)
  delay <- rgamma(n_sim, shape = args$shape, scale = args$scale)
  stime <- ptime + delay
  obs <- 2 * floor(stime[stime <= 40] / 2)
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
})

test_that(".meta_implied_moments returns the analytic moments when fully adjusted", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  expected_mean <- exp(args$meanlog + args$sdlog^2 / 2)
  expected_sd <- expected_mean * sqrt(exp(args$sdlog^2) - 1)
  expect_equal(moments[["mean"]], expected_mean, tolerance = 1e-8)
  expect_equal(moments[["sd"]], expected_sd, tolerance = 1e-8)
})

test_that(".meta_implied_moments matches Monte Carlo right truncated continuous moments", { # nolint: line_length_linter.
  set.seed(103)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 8
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  n_sim <- 5e5
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  obs <- delay[delay <= cutoff]
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.01)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
})

test_that(".meta_implied_moments applies the uniform single interval correction", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  adjusted <- .meta_implied_moments(
    "plnorm", args, cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  approx <- .meta_implied_moments(
    "plnorm", args, cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 2L, growth_rate = 0
  )
  expect_equal(approx[["mean"]], adjusted[["mean"]] + 0.5, tolerance = 1e-8)
  expect_equal(
    approx[["sd"]], sqrt(adjusted[["sd"]]^2 + 1 / 12), tolerance = 1e-8
  )
})

test_that(".meta_implied_moments truncates the uniform single interval estimand rather than shifting a truncated one", { # nolint: line_length_linter.
  set.seed(114)
  args <- list(meanlog = 1.8, sdlog = 0.5)
  cutoff <- 8
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0
  )
  n_sim <- 1e6
  observed <- runif(n_sim, 0, 1) + rlnorm(n_sim, args$meanlog, args$sdlog)
  observed <- observed[observed < cutoff]
  expect_equal(moments[["mean"]], mean(observed), tolerance = 0.005)
  expect_equal(moments[["sd"]], stats::sd(observed), tolerance = 0.01)
  # shifting the truncated continuous moments instead is materially biased
  truncated <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  expect_gt(truncated[["mean"]] + 0.5, moments[["mean"]] * 1.02)
})

test_that(".meta_implied_moments treats an underflowing primary censored CDF as zero", { # nolint: line_length_linter.
  args <- list(meanlog = 6, sdlog = 0.2)
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = 800, pwindow = 2, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0.15
  )
  expect_true(all(is.finite(moments)))
  expect_gt(moments[["mean"]], 300)
})

test_that(".meta_implied_prob matches the continuity corrected naive empirical CDF", { # nolint: line_length_linter.
  set.seed(104)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 30
  n_sim <- 2e5
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  obs <- floor(stime[stime <= floor(cutoff)])
  for (y in c(3, 5, 8, 12)) {
    prob <- .meta_implied_prob(
      y, "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    )
    expect_equal(
      prob, mean(obs < y) + mean(obs == y) / 2, tolerance = 0.02
    )
    expect_lt(prob, mean(obs <= y))
    expect_gt(prob, mean(obs < y))
  }
})

test_that(".meta_implied_prob interpolates the naive grid between its cells", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  prob <- function(y) {
    return(.meta_implied_prob(
      y, "plnorm", args, cutoff = 30, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    ))
  }
  mass <- .meta_grid_pmf("plnorm", args, 30, 1, 1, 0)
  expect_equal(prob(5.5), sum(mass[1:6]), tolerance = 1e-10)
  expect_equal(prob(5), sum(mass[1:5]) + mass[6] / 2, tolerance = 1e-10)
  expect_equal(prob(-0.5), 0, tolerance = 1e-10)
  expect_identical(prob(200), 1)
})

test_that(".meta_implied_prob returns the truncated continuous CDF when fully adjusted", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  prob <- .meta_implied_prob(
    9, "plnorm", args, cutoff = 12, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  expected <- stats::plnorm(9, args$meanlog, args$sdlog) /
    stats::plnorm(12, args$meanlog, args$sdlog)
  expect_equal(prob, expected, tolerance = 1e-8)
})

test_that(".meta_implied_moments returns the kurtosis of the naive estimand", {
  set.seed(106)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 30
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  n_sim <- 5e5
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  obs <- floor(stime[stime <= floor(cutoff)])
  centred <- obs - mean(obs)
  mc_kurtosis <- mean(centred^4) / mean(centred^2)^2
  expect_equal(moments[["kurtosis"]], mc_kurtosis, tolerance = 0.03)
})

test_that(".meta_implied_moments returns the analytic kurtosis when fully adjusted", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  s2 <- args$sdlog^2
  expected <- exp(4 * s2) + 2 * exp(3 * s2) + 3 * exp(2 * s2) - 3
  expect_equal(moments[["kurtosis"]], expected, tolerance = 1e-8)
})

test_that(".meta_implied_moments returns the kurtosis of a right truncated distribution", { # nolint: line_length_linter.
  set.seed(107)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 8
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  n_sim <- 5e5
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  obs <- delay[delay <= cutoff]
  centred <- obs - mean(obs)
  mc_kurtosis <- mean(centred^4) / mean(centred^2)^2
  expect_equal(moments[["kurtosis"]], mc_kurtosis, tolerance = 0.02)
})

test_that(".meta_summary_terms uses the kurtosis based standard error for reported sds", { # nolint: line_length_linter.
  set.seed(108)
  args <- list(meanlog = 1.5, sdlog = 0.5)
  cutoff <- 20
  study_n <- 200
  slots <- list(
    obs_type = 3L, study_n = study_n, trunc_adjusted = 0L,
    cens_adjusted = 0L, cutoff = cutoff, pwindow = 1, swindow = 1,
    value = 3, report_se = 0, quantile_p = 0, growth_rate = 0
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  n_sim <- 2e5
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  pool <- floor(stime[stime <= floor(cutoff)])
  sds <- vapply(
    seq_len(2000),
    function(i) stats::sd(sample(pool, study_n, replace = TRUE)),
    numeric(1)
  )
  expect_equal(terms[["se"]], stats::sd(sds), tolerance = 0.1)
  # The normal theory standard error ruled out by the design is much too
  # narrow for a skewed delay distribution.
  expect_gt(terms[["se"]], terms[["implied"]] / sqrt(2 * (study_n - 1)))
})

test_that(".meta_summary_terms uses a reported standard error when one is given", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    obs_type = 3L, study_n = 0L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 40, pwindow = 1, swindow = 1, value = 3, report_se = 0.42,
    quantile_p = 0, growth_rate = 0
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["se"]], 0.42)
})

test_that(".meta_implied_moments accounts for exponential growth in the primary window", { # nolint: line_length_linter.
  set.seed(105)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 30
  moments <- .meta_implied_moments(
    "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0.4
  )
  n_sim <- 2e5
  ptime <- primarycensored::rexpgrowth(n_sim, r = 0.4)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  obs <- floor(stime[stime <= floor(cutoff)])
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
})

test_that(".meta_grid_pmf returns a normalised grid of the expected length", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  mass <- .meta_grid_pmf(
    "plnorm", args, cutoff = 20, pwindow = 1, swindow = 1, growth_rate = 0
  )
  expect_length(mass, 20)
  expect_equal(sum(mass), 1, tolerance = 1e-12)
  expect_true(all(mass >= 0))
  wide <- .meta_grid_pmf(
    "plnorm", args, cutoff = 20, pwindow = 2, swindow = 2, growth_rate = 0
  )
  expect_length(wide, 10)
  expect_equal(sum(wide), 1, tolerance = 1e-12)
})

test_that(".meta_implied_moments matches the analytic gamma summaries", {
  args <- list(shape = 3, scale = 2)
  moments <- .meta_implied_moments(
    "pgamma", args, cutoff = 60, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  expect_equal(moments[["mean"]], 6, tolerance = 1e-8)
  expect_equal(moments[["sd"]], sqrt(12), tolerance = 1e-8)
  expect_equal(moments[["kurtosis"]], 3 + 6 / 3, tolerance = 1e-8)
})

test_that(".meta_implied_moments matches Monte Carlo weibull summaries", {
  set.seed(109)
  args <- list(shape = 1.7, scale = 8)
  moments <- .meta_implied_moments(
    "pweibull", args, cutoff = 100, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  delay <- rweibull(5e5, shape = args$shape, scale = args$scale)
  centred <- delay - mean(delay)
  expect_equal(moments[["mean"]], mean(delay), tolerance = 0.01)
  expect_equal(moments[["sd"]], stats::sd(delay), tolerance = 0.01)
  expect_equal(
    moments[["kurtosis"]],
    mean(centred^4) / mean(centred^2)^2,
    tolerance = 0.05
  )
})

test_that(".meta_implied_prob uses the primary censored CDF for the uniform single interval approximation", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  prob <- .meta_implied_prob(
    9, "plnorm", args, cutoff = 100, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 2L, growth_rate = 0
  )
  expected <- primarycensored::pprimarycensored(
    9, stats::plnorm, pwindow = 1, meanlog = args$meanlog, sdlog = args$sdlog
  )
  expect_equal(prob, expected, tolerance = 1e-8)
  # adding the primary offset shifts probability mass to longer delays, and
  # the exact CDF is close to but not equal to the half window shift
  expect_lt(prob, stats::plnorm(9, args$meanlog, args$sdlog))
  expect_equal(
    prob, stats::plnorm(8.5, args$meanlog, args$sdlog), tolerance = 1e-2
  )
})

test_that(".meta_implied_prob truncates the primary censored CDF when the study did not", { # nolint: line_length_linter.
  set.seed(115)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 10
  n_sim <- 2e5
  observed <- runif(n_sim, 0, 1) + rlnorm(n_sim, args$meanlog, args$sdlog)
  observed <- observed[observed < cutoff]
  for (y in c(3, 5, 8)) {
    prob <- .meta_implied_prob(
      y, "plnorm", args, cutoff = cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0
    )
    expect_equal(prob, mean(observed <= y), tolerance = 0.02)
  }
})

test_that(".meta_summary_terms uses the implied sd over root n for reported means", { # nolint: line_length_linter.
  set.seed(110)
  args <- list(meanlog = 1.5, sdlog = 0.5)
  cutoff <- 20
  study_n <- 200
  slots <- list(
    obs_type = 2L, study_n = study_n, trunc_adjusted = 0L,
    cens_adjusted = 0L, cutoff = cutoff, pwindow = 1, swindow = 1,
    value = 4, report_se = 0, quantile_p = 0, growth_rate = 0
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["observed"]], 4)
  n_sim <- 2e5
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  pool <- floor(stime[stime <= floor(cutoff)])
  means <- vapply(
    seq_len(2000),
    function(i) mean(sample(pool, study_n, replace = TRUE)),
    numeric(1)
  )
  expect_equal(terms[["implied"]], mean(pool), tolerance = 0.02)
  expect_equal(terms[["se"]], stats::sd(means), tolerance = 0.1)
})

test_that(".meta_summary_terms uses the binomial standard error for reported quantiles", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    obs_type = 4L, study_n = 250L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 60, pwindow = 1, swindow = 1, value = 6, report_se = 0,
    quantile_p = 0.75, growth_rate = 0
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["observed"]], 0.75)
  expect_equal(
    terms[["implied"]],
    stats::plnorm(6, args$meanlog, args$sdlog),
    tolerance = 1e-8
  )
  expect_equal(terms[["se"]], sqrt(0.75 * 0.25 / 250), tolerance = 1e-8)
})
