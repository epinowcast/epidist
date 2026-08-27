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
  # Study A reports a mean and a standard deviation, which share one row.
  expect_identical(nrow(prep_meta_estimates), nrow(sim_estimates) - 1L)
})

test_that("as_epidist_meta_model works with a mix of individual and summary data", { # nolint: line_length_linter.
  expect_s3_class(prep_meta_obs, "epidist_meta_model")
  expect_identical(
    nrow(prep_meta_obs), nrow(sim_obs) + nrow(sim_estimates) - 1L
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
    c(5L, 2L, 6L, 6L)
  )
  expect_true(all(summary_rows$delay_lwr == 0L))
  expect_true(all(summary_rows$n == 1))
  expect_identical(summary_rows$delay_upr, sim_estimates$value[-2])
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
  expect_identical(summary_rows$relative_obs_time, expected[-2])
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
  expect_true(all(paste0("vint", 1:8, "[n]") %in% family$vars))
  expect_true(all(paste0("vreal", 1:8, "[n]") %in% family$vars))
})

test_that("epidist_formula_model.epidist_meta_model binds the required slots", {
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family,
    formula = bf(mu ~ 1)
  )
  form <- as_string_formula(formula$formula)
  expect_true(grepl("delay_lwr", form, fixed = TRUE))
  expect_true(grepl("weights(n)", form, fixed = TRUE))
  expect_true(grepl(
    paste0(
      "vint(obs_type, study_n, trunc_adjusted, cens_adjusted, ",
      "trunc_design, group_start, group_len, chol_start)"
    ),
    form,
    fixed = TRUE
  ))
  expect_true(grepl(
    paste0(
      "vreal(relative_obs_time, pwindow, swindow, delay_upr, delay_min, ",
      "report_se, quantile_p, growth_rate)"
    ),
    form,
    fixed = TRUE
  ))
})

test_that("epidist_transform_data_model.epidist_meta_model aggregates only individual rows", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family,
    formula = bf(mu ~ 1)
  )
  trans <- suppressMessages(
    epidist_transform_data_model(prep_meta_obs, family, formula)
  )
  expect_s3_class(trans, "epidist_meta_model")
  expect_identical(
    sum(trans$obs_type != 1L), nrow(sim_estimates) - 1L
  )
  expect_lt(sum(trans$obs_type == 1L), nrow(sim_obs))
  expect_identical(sum(trans$n), sum(prep_meta_obs$n))
})

test_that("epidist_stancode.epidist_meta_model produces valid stanvars", {
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(
    prep_meta_obs, family,
    formula = bf(mu ~ 1)
  )
  stancode <- epidist_stancode(
    prep_meta_obs,
    family = family, formula = formula
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

# Numerical checks of the implied biased summaries against Monte Carlo
# simulation of the naive estimators they describe. The samples below are
# shared by every check of the same study design.

sim_naive_obs <- function(n, args, cutoff) {
  stime <- runif(n, 0, 1) + rlnorm(n, args$meanlog, args$sdlog)
  return(floor(stime[stime <= floor(cutoff)]))
}

sim_moments <- function(obs) {
  centred <- obs - mean(obs)
  return(c(
    mean = mean(obs),
    sd = stats::sd(obs),
    kurtosis = mean(centred^4) / mean(centred^2)^2,
    skewness = mean(centred^3) / mean(centred^2)^1.5
  ))
}

set.seed(101)
naive_args <- list(meanlog = 1.6, sdlog = 0.6)
naive_cutoff <- 30
naive_obs <- sim_naive_obs(5e5, naive_args, naive_cutoff)

test_that(".meta_implied_moments matches the Monte Carlo moments of a naive study", { # nolint: line_length_linter.
  moments <- .meta_implied_moments(
    "plnorm", naive_args,
    cutoff = naive_cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  simulated <- sim_moments(naive_obs)
  expect_equal(moments[["mean"]], simulated[["mean"]], tolerance = 0.02)
  expect_equal(moments[["sd"]], simulated[["sd"]], tolerance = 0.02)
  expect_equal(
    moments[["kurtosis"]], simulated[["kurtosis"]], tolerance = 0.03
  )
  expect_equal(
    moments[["skewness"]], simulated[["skewness"]], tolerance = 0.03
  )
})

test_that(".meta_implied_moments matches Monte Carlo naive discretisation with a wide secondary window", { # nolint: line_length_linter.
  set.seed(102)
  args <- list(shape = 2, scale = 2.5)
  cutoff <- 40
  moments <- .meta_implied_moments(
    "pgamma", args,
    cutoff = cutoff, pwindow = 2, swindow = 2,
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
    "plnorm", args,
    cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  s2 <- args$sdlog^2
  expected_mean <- exp(args$meanlog + s2 / 2)
  expect_equal(moments[["mean"]], expected_mean, tolerance = 1e-8)
  expect_equal(
    moments[["sd"]], expected_mean * sqrt(expm1(s2)), tolerance = 1e-8
  )
  expect_equal(
    moments[["kurtosis"]],
    exp(4 * s2) + 2 * exp(3 * s2) + 3 * exp(2 * s2) - 3,
    tolerance = 1e-8
  )
  expect_equal(
    moments[["skewness"]], (exp(s2) + 2) * sqrt(expm1(s2)), tolerance = 1e-8
  )
})

test_that(".meta_implied_moments matches Monte Carlo right truncated continuous moments", { # nolint: line_length_linter.
  set.seed(103)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 8
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  delay <- rlnorm(5e5, args$meanlog, args$sdlog)
  simulated <- sim_moments(delay[delay <= cutoff])
  expect_equal(moments[["mean"]], simulated[["mean"]], tolerance = 0.01)
  expect_equal(moments[["sd"]], simulated[["sd"]], tolerance = 0.02)
  expect_equal(
    moments[["kurtosis"]], simulated[["kurtosis"]], tolerance = 0.02
  )
})

test_that(".meta_implied_moments applies the uniform single interval correction", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  adjusted <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  approx <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 2L, growth_rate = 0
  )
  expect_equal(approx[["mean"]], adjusted[["mean"]] + 0.5, tolerance = 1e-8)
  expect_equal(
    approx[["sd"]], sqrt(adjusted[["sd"]]^2 + 1 / 12),
    tolerance = 1e-8
  )
})

test_that(".meta_implied_moments recovers a study that midpoints the primary and integrates the secondary", { # nolint: line_length_linter.
  # Code 4 summarises tau + U_p - pwindow / 2.
  set.seed(115)
  args <- list(meanlog = 1.8, sdlog = 0.5)
  n_sim <- 1e6
  for (pwindow in c(1, 3)) {
    for (obs_time in c(20, Inf)) {
      trunc_adjusted <- as.integer(is.infinite(obs_time))
      cutoff <- if (is.infinite(obs_time)) 80 else obs_time
      moments <- .meta_implied_moments(
        "plnorm", args,
        cutoff = cutoff, pwindow = pwindow, swindow = 1,
        trunc_adjusted = trunc_adjusted, cens_adjusted = 4L, growth_rate = 0
      )
      raw <- stats::runif(n_sim, 0, pwindow) +
        stats::rlnorm(n_sim, args$meanlog, args$sdlog)
      observed <- raw[raw <= obs_time] - pwindow / 2
      expect_equal(moments[["mean"]], mean(observed), tolerance = 0.01)
      expect_equal(moments[["sd"]], stats::sd(observed), tolerance = 0.01)
      # The implied distribution function must agree with the same simulation.
      reported <- stats::quantile(observed, c(0.25, 0.5, 0.9), names = FALSE)
      implied <- vapply(
        reported,
        .meta_implied_prob,
        numeric(1),
        dist = "plnorm", args = args, cutoff = cutoff, pwindow = pwindow,
        swindow = 1, trunc_adjusted = trunc_adjusted, cens_adjusted = 4L,
        growth_rate = 0
      )
      expect_equal(implied, c(0.25, 0.5, 0.9), tolerance = 0.01)
    }
  }
})

test_that("midpoint imputation of the primary event moves the uniform single interval estimand", { # nolint: line_length_linter.
  # Code 4 is code 2 moved down the delay axis by half a primary window, so
  # the mean moves and every other summary is untouched.
  args <- list(meanlog = 1.8, sdlog = 0.5)
  for (pwindow in c(1, 3)) {
    for (trunc_adjusted in c(0L, 1L)) {
      shared <- list(
        dist = "plnorm", args = args, cutoff = 30, pwindow = pwindow,
        swindow = 1, trunc_adjusted = trunc_adjusted, growth_rate = 0
      )
      uniform <- do.call(
        .meta_implied_moments, c(shared, list(cens_adjusted = 2L))
      )
      midpoint <- do.call(
        .meta_implied_moments, c(shared, list(cens_adjusted = 4L))
      )
      expect_equal(
        midpoint[["mean"]], uniform[["mean"]] - pwindow / 2, tolerance = 1e-10
      )
      expect_identical(midpoint[["sd"]], uniform[["sd"]])
      expect_identical(midpoint[["kurtosis"]], uniform[["kurtosis"]])
      expect_identical(midpoint[["skewness"]], uniform[["skewness"]])
      # The distribution function and the density move with it.
      expect_identical(
        do.call(.meta_implied_prob, c(
          list(y = 9 - pwindow / 2), shared, list(cens_adjusted = 4L)
        )),
        do.call(.meta_implied_prob, c(
          list(y = 9), shared, list(cens_adjusted = 2L)
        ))
      )
      expect_identical(
        do.call(.meta_implied_density, c(
          list(y = 9 - pwindow / 2), shared, list(cens_adjusted = 4L)
        )),
        do.call(.meta_implied_density, c(
          list(y = 9), shared, list(cens_adjusted = 2L)
        ))
      )
    }
  }
})

test_that("midpointing the primary event keeps the adjusted mean and widens the spread", { # nolint: line_length_linter.
  # Code 4 recentres the primary window, so it lands back on the fully
  # adjusted mean but still carries the window's variance.
  args <- list(meanlog = 1.8, sdlog = 0.5)
  pwindow <- 2
  shared <- list(
    dist = "plnorm", args = args, cutoff = 60, pwindow = pwindow, swindow = 1,
    trunc_adjusted = 1L, growth_rate = 0
  )
  adjusted <- do.call(
    .meta_implied_moments, c(shared, list(cens_adjusted = 1L))
  )
  midpoint <- do.call(
    .meta_implied_moments, c(shared, list(cens_adjusted = 4L))
  )
  expect_equal(midpoint[["mean"]], adjusted[["mean"]], tolerance = 1e-8)
  expect_equal(
    midpoint[["sd"]], sqrt(adjusted[["sd"]]^2 + pwindow^2 / 12),
    tolerance = 1e-8
  )
})

test_that(".meta_cens_base and .meta_cens_shift describe the midpoint codes", {
  expect_identical(
    vapply(0:4, .meta_cens_base, integer(1)), c(0L, 1L, 2L, 0L, 2L)
  )
  expect_identical(
    vapply(0:4, .meta_cens_shift, numeric(1), pwindow = 3, swindow = 2),
    c(0, 0, 0, 1, -1.5)
  )
})

test_that(".meta_implied_moments truncates the uniform single interval estimand rather than shifting a truncated one", { # nolint: line_length_linter.
  set.seed(114)
  args <- list(meanlog = 1.8, sdlog = 0.5)
  cutoff <- 8
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0
  )
  n_sim <- 1e6
  observed <- runif(n_sim, 0, 1) + rlnorm(n_sim, args$meanlog, args$sdlog)
  observed <- observed[observed < cutoff]
  expect_equal(moments[["mean"]], mean(observed), tolerance = 0.005)
  expect_equal(moments[["sd"]], stats::sd(observed), tolerance = 0.01)
  # shifting the truncated continuous moments instead is materially biased
  truncated <- .meta_implied_moments(
    "plnorm", args,
    cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  expect_gt(truncated[["mean"]] + 0.5, moments[["mean"]] * 1.02)
})

test_that(".meta_implied_moments treats an underflowing primary censored CDF as zero", { # nolint: line_length_linter.
  args <- list(meanlog = 6, sdlog = 0.2)
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 800, pwindow = 2, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0.15
  )
  expect_true(all(is.finite(moments)))
  expect_gt(moments[["mean"]], 300)
})

# A normaliser underflowing to exactly zero would turn a 0 / 0 division into
# NaN. A lognormal far above the grid gives that underflow exactly.

underflow_args <- list(meanlog = 100, sdlog = 0.1)

test_that("the naive grid returns a rejection sentinel when its mass underflows", { # nolint: line_length_linter.
  mass <- .meta_grid_pmf(
    "plnorm", underflow_args,
    cutoff = 5, pwindow = 1, swindow = 1, growth_rate = 0
  )
  expect_length(mass, 5)
  expect_true(all(is.na(mass)))
  moments <- .meta_implied_moments(
    "plnorm", underflow_args,
    cutoff = 5, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  expect_false(anyNA(moments))
  expect_true(all(is.infinite(moments[c("mean", "sd", "kurtosis")])))
  expect_identical(
    .meta_implied_prob(3, "plnorm", underflow_args, 0, 5, 1, 1, 0L, 0L, 0), Inf
  )
  expect_identical(
    .meta_implied_density(3, "plnorm", underflow_args, 0, 5, 1, 1, 0L, 0L, 0),
    Inf
  )
})

test_that("a truncated continuous estimand rejects an underflowing normaliser", { # nolint: line_length_linter.
  moments <- .meta_implied_moments(
    "plnorm", underflow_args,
    cutoff = 5, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  expect_false(anyNA(moments))
  expect_true(all(is.infinite(moments[c("mean", "sd", "kurtosis")])))
  expect_identical(
    .meta_implied_prob(3, "plnorm", underflow_args, 0, 5, 1, 1, 0L, 1L, 0), Inf
  )
  expect_identical(
    .meta_implied_density(6, "plnorm", underflow_args, 0, 30, 1, 1, 0L, 1L, 0),
    Inf
  )
})

test_that(".meta_summary_terms produces a -Inf log likelihood rather than NaN when a normaliser underflows", { # nolint: line_length_linter.
  slots <- list(
    lower = 0,
    obs_type = 2L, study_n = 50L, trunc_adjusted = 0L, cens_adjusted = 0L,
    cutoff = 5, pwindow = 1, swindow = 1, value = 3, report_se = 0,
    quantile_p = 0, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", underflow_args)
  log_lik <- stats::dnorm(
    terms[["observed"]], terms[["implied"]], terms[["se"]],
    log = TRUE
  )
  expect_false(is.nan(log_lik))
  expect_identical(log_lik, -Inf)
})

# A wide grid runs into the tail, where the distribution function saturates at
# one and only part of the grid underflows. Differencing it there returns
# cells of order -1e-13, which the total underflow guard above does not catch.

test_that("the naive grid stays a valid pmf on a grid that runs into the tail", { # nolint: line_length_linter.
  saturating <- list(
    list(dist = "plnorm", args = list(meanlog = 1.6, sdlog = 0.6),
         cutoff = 400, accrual = 0L),
    list(dist = "plnorm", args = list(meanlog = 1, sdlog = 0.4),
         cutoff = 100, accrual = 0L),
    list(dist = "plnorm", args = list(meanlog = 1.6, sdlog = 0.6),
         cutoff = 400, accrual = 1L),
    list(dist = "pgamma", args = list(shape = 4, scale = 1),
         cutoff = 50, accrual = 0L)
  )
  for (case in saturating) {
    mass <- .meta_grid_pmf(
      case$dist, case$args,
      cutoff = case$cutoff, pwindow = 1, swindow = 1, growth_rate = 0,
      accrual = case$accrual
    )
    expect_length(mass, case$cutoff)
    expect_false(anyNA(mass))
    expect_gte(min(mass), 0)
    expect_equal(sum(mass), 1, tolerance = 1e-9)
    # An invalid pmf is not just untidy: sampling from it errors.
    expect_no_error(sample(seq_along(mass), 1, prob = mass))
  }
})

test_that(".meta_implied_prob matches the continuity corrected naive empirical CDF", { # nolint: line_length_linter.
  for (y in c(3, 5, 8, 12)) {
    prob <- .meta_implied_prob(
      y, "plnorm", naive_args,
      cutoff = naive_cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    )
    expect_equal(
      prob, mean(naive_obs < y) + mean(naive_obs == y) / 2,
      tolerance = 0.02
    )
    expect_lt(prob, mean(naive_obs <= y))
    expect_gt(prob, mean(naive_obs < y))
  }
})

test_that(".meta_implied_prob interpolates the naive grid between its cells", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  prob <- function(y) {
    return(.meta_implied_prob(
      y, "plnorm", args,
      cutoff = 30, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    ))
  }
  mass <- .meta_grid_pmf("plnorm", args, 0, 30, 1, 1, 0)
  expect_equal(prob(5.5), sum(mass[1:6]), tolerance = 1e-10)
  expect_equal(prob(5), sum(mass[1:5]) + mass[6] / 2, tolerance = 1e-10)
  expect_equal(prob(-0.5), 0, tolerance = 1e-10)
  expect_identical(prob(200), 1)
})

test_that(".meta_implied_prob normalises the continuous CDF by the study cutoff", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  expect_equal(
    .meta_implied_prob(
      9, "plnorm", args,
      cutoff = 12, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
    ),
    stats::plnorm(9, args$meanlog, args$sdlog) /
      stats::plnorm(12, args$meanlog, args$sdlog),
    tolerance = 1e-8
  )
  # A study that adjusted for truncation reports the untruncated CDF.
  expect_equal(
    .meta_implied_prob(
      9, "plnorm", args,
      cutoff = 12, pwindow = 1, swindow = 1,
      trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
    ),
    stats::plnorm(9, args$meanlog, args$sdlog),
    tolerance = 1e-8
  )
})


test_that(".meta_summary_terms uses the kurtosis based standard error for reported sds", { # nolint: line_length_linter.
  set.seed(108)
  args <- list(meanlog = 1.5, sdlog = 0.5)
  cutoff <- 20
  study_n <- 200
  slots <- list(
    lower = 0,
    obs_type = 3L, study_n = study_n, trunc_adjusted = 0L,
    cens_adjusted = 0L, cutoff = cutoff, pwindow = 1, swindow = 1,
    value = 3, report_se = 0, quantile_p = 0, growth_rate = 0,
    trunc_design = 0L
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
    lower = 0,
    obs_type = 3L, study_n = 0L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 40, pwindow = 1, swindow = 1, value = 3, report_se = 0.42,
    quantile_p = 0, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["se"]], 0.42)
})

test_that(".meta_implied_moments accounts for exponential growth in the primary window", { # nolint: line_length_linter.
  set.seed(105)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 30
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0.4
  )
  n_sim <- 2e5
  ptime <- primarycensored::rexpgrowth(n_sim, r = 0.4)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  stime <- ptime + delay
  obs <- floor(stime[stime <= floor(cutoff)])
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
})

test_that(".meta_grid_pmf lays out one normalised cell per secondary window", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  mass <- .meta_grid_pmf(
    "plnorm", args,
    cutoff = 20, pwindow = 1, swindow = 1, growth_rate = 0
  )
  expect_length(mass, 20)
  expect_true(all(mass >= 0))
  cdf <- .meta_pcens_cdf(seq_len(20), "plnorm", args, 1, 0)
  expect_equal(cumsum(mass), cdf / cdf[20], tolerance = 1e-12)
  wide <- .meta_grid_pmf(
    "plnorm", args,
    cutoff = 20, pwindow = 2, swindow = 2, growth_rate = 0
  )
  expect_length(wide, 10)
  wide_cdf <- .meta_pcens_cdf(seq(2, 20, by = 2), "plnorm", args, 2, 0)
  expect_equal(cumsum(wide), wide_cdf / wide_cdf[10], tolerance = 1e-12)
})

test_that(".meta_implied_moments matches the analytic gamma summaries", {
  args <- list(shape = 3, scale = 2)
  moments <- .meta_implied_moments(
    "pgamma", args,
    cutoff = 60, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  expect_equal(moments[["mean"]], 6, tolerance = 1e-8)
  expect_equal(moments[["sd"]], sqrt(12), tolerance = 1e-8)
  expect_equal(moments[["kurtosis"]], 3 + 6 / 3, tolerance = 1e-8)
  expect_equal(moments[["skewness"]], 2 / sqrt(3), tolerance = 1e-8)
})

test_that(".meta_implied_moments matches Monte Carlo weibull summaries", {
  set.seed(109)
  args <- list(shape = 1.7, scale = 8)
  moments <- .meta_implied_moments(
    "pweibull", args,
    cutoff = 100, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  simulated <- sim_moments(
    rweibull(5e5, shape = args$shape, scale = args$scale)
  )
  expect_equal(moments[["mean"]], simulated[["mean"]], tolerance = 0.01)
  expect_equal(moments[["sd"]], simulated[["sd"]], tolerance = 0.01)
  expect_equal(
    moments[["kurtosis"]], simulated[["kurtosis"]], tolerance = 0.05
  )
  expect_equal(
    moments[["skewness"]], simulated[["skewness"]], tolerance = 0.02
  )
})

test_that(".meta_implied_prob uses the primary censored CDF for the uniform single interval approximation", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  prob <- .meta_implied_prob(
    9, "plnorm", args,
    cutoff = 100, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 2L, growth_rate = 0
  )
  expected <- primarycensored::pprimarycensored(
    9, stats::plnorm,
    pwindow = 1, meanlog = args$meanlog, sdlog = args$sdlog
  )
  expect_equal(prob, expected, tolerance = 1e-8)
  # adding the primary offset shifts probability mass to longer delays, and
  # the exact CDF is close to but not equal to the half window shift
  expect_lt(prob, stats::plnorm(9, args$meanlog, args$sdlog))
  expect_equal(
    prob, stats::plnorm(8.5, args$meanlog, args$sdlog),
    tolerance = 1e-2
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
      y, "plnorm", args,
      cutoff = cutoff, pwindow = 1, swindow = 1,
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
    lower = 0,
    obs_type = 2L, study_n = study_n, trunc_adjusted = 0L,
    cens_adjusted = 0L, cutoff = cutoff, pwindow = 1, swindow = 1,
    value = 4, report_se = 0, quantile_p = 0, growth_rate = 0,
    trunc_design = 0L
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
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 60, pwindow = 1, swindow = 1, value = 6, report_se = 0,
    quantile_p = 0.75, growth_rate = 0, trunc_design = 0L
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

# Accrual truncation, midpoint imputation and delay scale quantile standard
# errors, checked against direct simulation of the processes they describe.

sim_accrual_ptime <- function(n, window, growth_rate) {
  u <- runif(n)
  if (growth_rate == 0) {
    return(u * window)
  }
  return(log1p(u * expm1(growth_rate * window)) / growth_rate)
}

test_that(".meta_implied_moments matches Monte Carlo accrual truncation without growth", { # nolint: line_length_linter.
  set.seed(120)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 30
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = window, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0,
    trunc_design = 1L
  )
  n_sim <- 1e6
  ptime <- sim_accrual_ptime(n_sim, window, 0)
  stime <- ptime + rlnorm(n_sim, args$meanlog, args$sdlog)
  keep <- stime <= window
  obs <- floor(stime[keep]) - floor(ptime[keep])
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.03)
})

test_that(".meta_implied_moments matches Monte Carlo accrual truncation with growth", { # nolint: line_length_linter.
  set.seed(121)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 30
  growth_rate <- 0.2
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = window, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = growth_rate,
    trunc_design = 1L
  )
  n_sim <- 1e6
  ptime <- sim_accrual_ptime(n_sim, window, growth_rate)
  stime <- ptime + rlnorm(n_sim, args$meanlog, args$sdlog)
  keep <- stime <= window
  obs <- floor(stime[keep]) - floor(ptime[keep])
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.03)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.04)
})

test_that(".meta_implied_moments matches Monte Carlo accrual truncation of a continuous estimand", { # nolint: line_length_linter.
  set.seed(122)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 20
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = window, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 1L
  )
  n_sim <- 1e6
  ptime <- sim_accrual_ptime(n_sim, window, 0)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  obs <- delay[ptime + delay <= window]
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.01)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
})

test_that(".meta_implied_moments matches Monte Carlo accrual truncation of the uniform single interval approximation", { # nolint: line_length_linter.
  set.seed(127)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 15
  pwindow <- 2
  moments <- .meta_implied_moments(
    "plnorm", args,
    cutoff = window, pwindow = pwindow, swindow = pwindow,
    trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0,
    trunc_design = 1L
  )
  n_sim <- 2e6
  ptime <- sim_accrual_ptime(n_sim, window, 0)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  keep <- ptime + delay <= window
  # The study left the primary interval uncorrected, so it summarised the time
  # from the start of the primary censoring window to the secondary event.
  obs <- ptime[keep] - pwindow * floor(ptime[keep] / pwindow) + delay[keep]
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.01)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.01)
})

test_that(".meta_implied_moments accrual weight offsets the primary window only for the uniform single interval approximation", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  cdf <- .meta_pcens_cdf(
    seq(0, 15, length.out = .meta_n_quad() + 1), "plnorm", args, 2, 0
  )
  # A positive offset leaves more follow up for long delays, so it shifts the
  # reweighted distribution function down.
  offset <- .meta_accrual_reweight(cdf, 0, 15, 0, weight_offset = 1)[-1]
  plain <- .meta_accrual_reweight(cdf, 0, 15, 0)[-1]
  expect_true(all(offset <= plain))
  expect_true(any(offset < plain))
  continuous <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 15, pwindow = 2, swindow = 2,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 1L
  )
  quad <- seq(0, 15, length.out = .meta_n_quad() + 1)
  unoffset <- .meta_survival_moments(
    .meta_accrual_reweight(
      do.call(.pdist("plnorm"), c(list(q = quad), args)), 0, 15, 0
    ),
    0, 15
  )
  expect_identical(continuous, unoffset)
})

test_that(".meta_implied_prob matches the accrual truncated uniform single interval CDF", { # nolint: line_length_linter.
  set.seed(128)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 15
  n_sim <- 2e6
  ptime <- sim_accrual_ptime(n_sim, window, 0)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  keep <- ptime + delay <= window
  obs <- ptime[keep] - floor(ptime[keep]) + delay[keep]
  for (y in c(4, 6, 9)) {
    prob <- .meta_implied_prob(
      y, "plnorm", args,
      cutoff = window, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 2L, growth_rate = 0,
      trunc_design = 1L
    )
    expect_equal(prob, mean(obs <= y), tolerance = 0.01)
  }
})

test_that(".meta_implied_moments accrual truncation is more severe than cohort truncation", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  cohort <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 20, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 0L
  )
  accrual <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 20, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 1L
  )
  expect_lt(accrual[["mean"]], cohort[["mean"]])
})

test_that(".meta_implied_moments accrual truncation is continuous in the growth rate", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  zero <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 20, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 1L
  )
  tiny <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 20, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 1e-8,
    trunc_design = 1L
  )
  expect_equal(zero[["mean"]], tiny[["mean"]], tolerance = 1e-6)
  expect_equal(zero[["sd"]], tiny[["sd"]], tolerance = 1e-6)
})

test_that(".meta_implied_moments ignores the truncation design when the study adjusted", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  cohort <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 0L, growth_rate = 0,
    trunc_design = 0L
  )
  accrual <- .meta_implied_moments(
    "plnorm", args,
    cutoff = 50, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 0L, growth_rate = 0,
    trunc_design = 1L
  )
  expect_identical(cohort, accrual)
})

test_that(".meta_implied_prob matches the accrual truncated empirical CDF", {
  set.seed(123)
  args <- list(meanlog = 1.6, sdlog = 0.5)
  window <- 20
  n_sim <- 5e5
  ptime <- sim_accrual_ptime(n_sim, window, 0)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  obs <- delay[ptime + delay <= window]
  for (y in c(3, 5, 8)) {
    prob <- .meta_implied_prob(
      y, "plnorm", args,
      cutoff = window, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
      trunc_design = 1L
    )
    expect_equal(prob, mean(obs <= y), tolerance = 0.02)
  }
})

test_that(".meta_implied_moments matches Monte Carlo midpoint imputation", {
  moments <- .meta_implied_moments(
    "plnorm", naive_args,
    cutoff = naive_cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 3L, growth_rate = 0
  )
  # Code 3 assigns each delay to the centre of the interval it was seen in.
  obs <- naive_obs + 0.5
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
})

test_that(".meta_implied_moments shifts the naive grid by half a secondary window", { # nolint: line_length_linter.
  args <- list(shape = 2, scale = 3)
  naive <- .meta_implied_moments(
    "pgamma", args,
    cutoff = 40, pwindow = 2, swindow = 2,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  midpoint <- .meta_implied_moments(
    "pgamma", args,
    cutoff = 40, pwindow = 2, swindow = 2,
    trunc_adjusted = 0L, cens_adjusted = 3L, growth_rate = 0
  )
  expect_equal(midpoint[["mean"]], naive[["mean"]] + 1, tolerance = 1e-10)
  expect_identical(midpoint[["sd"]], naive[["sd"]])
  expect_identical(midpoint[["kurtosis"]], naive[["kurtosis"]])
})

test_that(".meta_implied_prob shifts the naive grid CDF for midpoint imputation", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  shifted <- .meta_implied_prob(
    6.5, "plnorm", args,
    cutoff = 30, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 3L, growth_rate = 0
  )
  unshifted <- .meta_implied_prob(
    6, "plnorm", args,
    cutoff = 30, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  expect_identical(shifted, unshifted)
})

test_that(".meta_implied_prob matches the midpoint imputed empirical CDF", {
  obs <- naive_obs + 0.5
  for (y in c(3.5, 5.5, 8.5)) {
    prob <- .meta_implied_prob(
      y, "plnorm", naive_args,
      cutoff = naive_cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 3L, growth_rate = 0
    )
    expect_equal(
      prob, mean(obs < y) + mean(obs == y) / 2,
      tolerance = 0.02
    )
  }
})

test_that(".meta_summary_terms converts a delay scale quantile se by the delta method", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 60, pwindow = 1, swindow = 1, value = 6, report_se = 0.4,
    quantile_p = 0.75, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_equal(
    terms[["se"]],
    stats::dlnorm(6, args$meanlog, args$sdlog) * 0.4,
    tolerance = 1e-4
  )
})

test_that(".meta_summary_terms uses the grid mass as the density for a naive study", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 0L, cens_adjusted = 0L,
    cutoff = 20, pwindow = 1, swindow = 1, value = 5, report_se = 0.6,
    quantile_p = 0.6, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  mass <- .meta_grid_pmf("plnorm", args, 0, 20, 1, 1, 0)
  expect_equal(terms[["se"]], mass[6] * 0.6, tolerance = 1e-10)
})

test_that(".meta_summary_terms guards a delay scale quantile se away from zero", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 0L, cens_adjusted = 0L,
    cutoff = 20, pwindow = 1, swindow = 1, value = 19.6, report_se = 0.6,
    quantile_p = 0.99, growth_rate = 0, trunc_design = 0L
  )
  # The reported value sits in the top grid cell, where the density is zero.
  expect_identical(
    .meta_implied_density(
      slots$value, "plnorm", args, 0, slots$cutoff, 1, 1, 0L, 0L, 0
    ),
    0
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["se"]], .meta_min_prob_se())
  # Inside the grid the delta method is used untouched.
  slots$value <- 19
  expect_gt(.meta_summary_terms(slots, "plnorm", args)[["se"]], 1e-4)
})

test_that(".meta_summary_terms delta method matches a bootstrapped quantile se", { # nolint: line_length_linter.
  set.seed(126)
  args <- list(meanlog = 1.5, sdlog = 0.5)
  cutoff <- 20
  study_n <- 200
  pool <- rlnorm(2e5, args$meanlog, args$sdlog)
  pool <- pool[pool <= cutoff]
  y <- stats::median(pool)
  boot <- vapply(
    seq_len(2000),
    function(i) {
      draw <- sample(pool, study_n, replace = TRUE)
      return(c(stats::median(draw), mean(draw <= y)))
    },
    numeric(2)
  )
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = study_n, trunc_adjusted = 0L, cens_adjusted = 1L,
    cutoff = cutoff, pwindow = 1, swindow = 1, value = y,
    report_se = stats::sd(boot[1, ]), quantile_p = 0.5, growth_rate = 0,
    trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_equal(terms[["se"]], stats::sd(boot[2, ]), tolerance = 0.1)
})

test_that(".meta_summary_terms keeps the binomial se when no quantile se is given", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 60, pwindow = 1, swindow = 1, value = 6, report_se = 0,
    quantile_p = 0.75, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_equal(terms[["se"]], sqrt(0.75 * 0.25 / 250), tolerance = 1e-8)
})

# Joint likelihoods for several summaries reported by one study.

test_that(".meta_moment_correlation is the skewness over the excess kurtosis", { # nolint: line_length_linter.
  grid <- expand.grid(meanlog = c(0.5, 1.5, 2.5), sdlog = c(0.2, 0.6, 1.2))
  for (i in seq_len(nrow(grid))) {
    moments <- .meta_implied_moments(
      "plnorm", list(meanlog = grid$meanlog[i], sdlog = grid$sdlog[i]),
      cutoff = 30, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    )
    expect_equal(
      .meta_moment_correlation(moments),
      moments[["skewness"]] / sqrt(moments[["kurtosis"]] - 1),
      tolerance = 1e-10
    )
  }
  # Grid and quadrature moments can break the kurtosis skewness bound, which
  # is the only case the clamp is there for.
  degenerate <- .meta_moment_vector(5, 4, 1e4, 3 * 4^2)
  expect_identical(
    .meta_moment_correlation(degenerate), .meta_max_correlation()
  )
  expect_identical(
    .meta_moment_correlation(.meta_moment_vector(5, 4, -1e4, 3 * 4^2)),
    -.meta_max_correlation()
  )
})

test_that(".meta_moment_pair_ll uses a positive definite covariance matrix", {
  moments <- .meta_implied_moments(
    "plnorm", list(meanlog = 1.6, sdlog = 0.6),
    cutoff = 30, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  study_n <- 80
  se_mean <- moments[["sd"]] / sqrt(study_n)
  se_sd <- .meta_sd_se(moments, study_n)
  rho <- .meta_moment_correlation(moments)
  covariance <- matrix(
    c(
      se_mean^2, rho * se_mean * se_sd,
      rho * se_mean * se_sd, se_sd^2
    ),
    nrow = 2
  )
  expect_true(all(eigen(covariance, only.values = TRUE)$values > 0))
  offset <- c(0.2, -0.1)
  expected <- -log(2 * pi) -
    0.5 * log(det(covariance)) -
    0.5 * drop(t(offset) %*% solve(covariance) %*% offset)
  expect_equal(
    .meta_moment_pair_ll(
      moments[["mean"]] + offset[1], moments[["sd"]] + offset[2], moments,
      study_n
    ),
    expected,
    tolerance = 1e-8
  )
})

test_that(".meta_moment_pair_ll is two independent normals when the estimand is symmetric", { # nolint: line_length_linter.
  moments <- .meta_moment_vector(5, 4, 0, 3 * 4^2)
  study_n <- 50
  joint <- .meta_moment_pair_ll(5.4, 1.8, moments, study_n)
  independent <- stats::dnorm(
    5.4, moments[["mean"]], moments[["sd"]] / sqrt(study_n),
    log = TRUE
  ) +
    stats::dnorm(
      1.8, moments[["sd"]], .meta_sd_se(moments, study_n),
      log = TRUE
    )
  expect_equal(joint, independent, tolerance = 1e-10)
})

test_that(".meta_moment_pair_ll rejects when a normaliser underflows", {
  expect_identical(
    .meta_moment_pair_ll(5, 2, .meta_moment_failure(), 50), -Inf
  )
})

test_that(".meta_quantile_counts round to non decreasing cumulative counts", {
  expect_identical(
    .meta_quantile_counts(c(0.25, 0.5, 0.75), 30), c(8L, 15L, 22L)
  )
  # Probabilities that round to the same count must not give a negative cell.
  expect_identical(.meta_quantile_counts(c(0.5, 0.51, 0.99), 7), c(4L, 4L, 7L))
  expect_identical(.meta_quantile_counts(c(0.01, 0.999), 5), c(0L, 5L))
})

test_that(".meta_quantile_set_ll matches the multinomial mass of its cells", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  y <- c(3, 5, 8)
  counts <- .meta_quantile_counts(c(0.25, 0.5, 0.75), 40)
  prob <- stats::plnorm(y, args$meanlog, args$sdlog)
  cells <- diff(c(0, prob, 1))
  expected <- stats::dmultinom(
    diff(c(0L, counts, 40L)),
    prob = cells, log = TRUE
  )
  expect_equal(
    .meta_quantile_set_ll(y, counts, 40, "plnorm", args, slots),
    expected,
    tolerance = 1e-10
  )
})

test_that(".meta_quantile_set_ll is the binomial mass for a single quantile", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  prob <- stats::plnorm(5, args$meanlog, args$sdlog)
  expect_equal(
    .meta_quantile_set_ll(5, 100L, 200, "plnorm", args, slots),
    stats::dbinom(100, 200, prob, log = TRUE),
    tolerance = 1e-10
  )
})

test_that(".meta_quantile_set_ll rejects a cell the study saw but the estimand cannot reach", { # nolint: line_length_linter.
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  expect_identical(
    .meta_quantile_set_ll(
      1e-8, 50L, 100, "plnorm", list(meanlog = 5, sdlog = 0.1), slots
    ),
    -Inf
  )
})

test_that("the joint quantile likelihood agrees with independent normal terms for a large near symmetric study", { # nolint: line_length_linter.
  # A single quantile of a large study is where the multinomial and the normal
  # approximation to it should be indistinguishable up to a constant.
  args_a <- list(meanlog = 1.6, sdlog = 0.6)
  args_b <- list(meanlog = 1.62, sdlog = 0.6)
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L,
    obs_type = 4L, study_n = 20000, value = 5, quantile_p = 0.5,
    report_se = 0
  )
  counts <- .meta_quantile_counts(0.5, slots$study_n)
  joint_drop <- .meta_quantile_set_ll(
    5, counts, slots$study_n, "plnorm", args_a, slots
  ) -
    .meta_quantile_set_ll(5, counts, slots$study_n, "plnorm", args_b, slots)
  normal_ll <- function(args) {
    summaries <- .meta_summary_terms(slots, "plnorm", args)
    return(stats::dnorm(
      summaries[["observed"]], summaries[["implied"]], summaries[["se"]],
      log = TRUE
    ))
  }
  normal_drop <- normal_ll(args_a) - normal_ll(args_b)
  expect_equal(joint_drop, normal_drop, tolerance = 0.02)
})

test_that("the joint quantile likelihood down weights a median with an interquartile range from a small study", { # nolint: line_length_linter.
  args_a <- list(meanlog = 1.6, sdlog = 0.6)
  args_b <- list(meanlog = 1.75, sdlog = 0.6)
  study_n <- 30
  p <- c(0.25, 0.5, 0.75)
  y <- stats::qlnorm(p, args_a$meanlog, args_a$sdlog)
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L,
    obs_type = 4L, study_n = study_n, report_se = 0
  )
  counts <- .meta_quantile_counts(p, study_n)
  joint_drop <- .meta_quantile_set_ll(
    y, counts, study_n, "plnorm", args_a, slots
  ) -
    .meta_quantile_set_ll(y, counts, study_n, "plnorm", args_b, slots)
  independent_ll <- function(args) {
    return(sum(vapply(
      seq_along(p),
      function(j) {
        one <- c(slots, list(value = y[j], quantile_p = p[j]))
        summaries <- .meta_summary_terms(one, "plnorm", args)
        return(stats::dnorm(
          summaries[["observed"]], summaries[["implied"]], summaries[["se"]],
          log = TRUE
        ))
      },
      numeric(1)
    )))
  }
  independent_drop <- independent_ll(args_a) - independent_ll(args_b)
  expect_lt(joint_drop, independent_drop)
})

test_that("the joint moment likelihood down weights a mean and standard deviation from a small study", { # nolint: line_length_linter.
  moments_a <- .meta_implied_moments(
    "plnorm", list(meanlog = 1.6, sdlog = 0.6),
    cutoff = 60, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  moments_b <- .meta_implied_moments(
    "plnorm", list(meanlog = 1.72, sdlog = 0.6),
    cutoff = 60, pwindow = 1, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
  )
  study_n <- 25
  y_mean <- moments_a[["mean"]]
  y_sd <- moments_a[["sd"]]
  joint_drop <- .meta_moment_pair_ll(y_mean, y_sd, moments_a, study_n) -
    .meta_moment_pair_ll(y_mean, y_sd, moments_b, study_n)
  independent_ll <- function(moments) {
    return(
      stats::dnorm(
        y_mean, moments[["mean"]], moments[["sd"]] / sqrt(study_n),
        log = TRUE
      ) +
        stats::dnorm(
          y_sd, moments[["sd"]], .meta_sd_se(moments, study_n),
          log = TRUE
        )
    )
  }
  independent_drop <- independent_ll(moments_a) - independent_ll(moments_b)
  expect_lt(joint_drop, independent_drop)
})

test_that("as_epidist_meta_model groups a mean and standard deviation from one study", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"),
    type = c("sd", "mean"),
    value = c(3.6, 7.5),
    n = c(120, 120),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(nrow(meta), 1L)
  expect_identical(meta$obs_type, 5L)
  expect_identical(meta$group_len, 2L)
  # The mean is stored first so that the bivariate normal knows which is which.
  expect_identical(.meta_members(meta)$value, c(7.5, 3.6))
})

test_that("as_epidist_meta_model groups the quantiles of one study into a set", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A",
    type = "quantile",
    value = c(9.4, 4.2, 6.1),
    p = c(0.75, 0.25, 0.5),
    n = 60,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(nrow(meta), 1L)
  expect_identical(meta$obs_type, 6L)
  expect_identical(meta$group_len, 3L)
  expect_identical(.meta_members(meta)$value, c(4.2, 6.1, 9.4))
  expect_identical(.meta_members(meta)$count, c(15L, 30L, 45L))
})

test_that("as_epidist_meta_model keeps a single quantile fittable on its own", {
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = "quantile", value = 6.1, p = 0.5, n = 60,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, 6L)
  expect_identical(meta$group_len, 1L)
})

test_that("as_epidist_meta_model leaves a summary with a reported standard error ungrouped", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"),
    type = c("mean", "sd"),
    value = c(7.5, 3.6),
    se = c(0.3, NA),
    n = c(120, 120),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(2L, 3L))
  expect_true(all(meta$group_len == 0L))
})

test_that("as_epidist_meta_model does not group summaries that differ in a covariate", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"),
    type = c("mean", "sd"),
    value = c(7.5, 3.6),
    n = c(120, 120),
    setting = c("hospital", "community"),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(2L, 3L))
})

test_that("as_epidist_meta_model does not group summaries that differ in their study metadata", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A"),
    type = c("mean", "sd"),
    value = c(7.5, 3.6),
    n = c(120, 120),
    relative_obs_time = c(20, 30),
    trunc_adjusted = FALSE,
    max_delay = 100,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(2L, 3L))
})

test_that("as_epidist_meta_model splits a repeated summary type into its own group", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A",
    type = c("mean", "mean", "sd"),
    value = c(7.5, 8.1, 3.6),
    n = 120,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(5L, 2L))
  expect_identical(.meta_members(meta)$value, c(7.5, 3.6))
})

test_that("as_epidist_meta_model errors on quantiles that do not increase with their probability", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A",
    type = "quantile",
    value = c(6.1, 4.2),
    p = c(0.25, 0.5),
    n = 60,
    stringsAsFactors = FALSE
  )))
  expect_error(
    suppressMessages(as_epidist_meta_model(estimates = estimates)),
    "must increase"
  )
})

test_that("as_epidist_meta_model errors on two quantiles at the same probability", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A",
    type = "quantile",
    value = c(4.2, 6.1),
    p = c(0.5, 0.5),
    n = 60,
    stringsAsFactors = FALSE
  )))
  expect_error(
    suppressMessages(as_epidist_meta_model(estimates = estimates)),
    "same probability"
  )
})

test_that("assert_epidist.epidist_meta_model checks the grouped summary members", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A",
    type = c("mean", "sd"),
    value = c(7.5, 3.6),
    n = 120,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  broken <- meta
  broken$group_len <- 1L
  expect_error(assert_epidist(broken), "exactly two")
  overrun <- meta
  overrun$group_start <- 2L
  expect_error(assert_epidist(overrun), "index within")
  empty <- meta
  empty <- .meta_set_members(empty, .meta_empty_members())
  expect_error(assert_epidist(empty), "index within")
})

test_that("epidist_stancode.epidist_meta_model passes the grouped members to Stan", { # nolint: line_length_linter.
  family <- epidist_family(prep_meta_obs, family = lognormal())
  formula <- epidist_formula(prep_meta_obs, family, formula = bf(mu ~ 1))
  stancode <- epidist_stancode(
    prep_meta_obs,
    family = family, formula = formula
  )
  scode <- paste(vapply(stancode, function(x) x$scode, character(1)),
    collapse = "\n"
  )
  expect_true(grepl("int<lower=0> N_meta_group;", scode, fixed = TRUE))
  expect_true(
    grepl("vector[N_meta_group] meta_group_value;", scode, fixed = TRUE)
  )
  expect_true(
    grepl("array[N_meta_group] int meta_group_count;", scode, fixed = TRUE)
  )
  expect_true(
    grepl("meta_lognormal_quantile_set_lpmf", scode, fixed = TRUE)
  )
  expect_true(
    grepl("meta_lognormal_moment_pair_lpdf", scode, fixed = TRUE)
  )
})

test_that(".meta_row_log_lik dispatches on the observation type", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  base <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L, study_n = 60,
    report_se = 0, quantile_p = 0, value = 6
  )
  pair <- c(base, list(
    obs_type = 5L, group_value = c(6, 3),
    group_count = c(0, 0)
  ))
  moments <- .meta_implied_moments(
    "plnorm", args, base$lower, base$cutoff, base$pwindow, base$swindow,
    base$trunc_adjusted, base$cens_adjusted, base$growth_rate,
    base$trunc_design
  )
  expect_equal(
    .meta_row_log_lik(pair, "plnorm", args),
    .meta_moment_pair_ll(6, 3, moments, 60),
    tolerance = 1e-10
  )
  qset <- c(base, list(
    obs_type = 6L, group_value = c(4, 6),
    group_count = c(15, 30), quantile_p = 0.25
  ))
  expect_equal(
    .meta_row_log_lik(qset, "plnorm", args),
    .meta_quantile_set_ll(c(4, 6), c(15, 30), 60, "plnorm", args, qset),
    tolerance = 1e-10
  )
})

test_that(".meta_grid_prob takes the three evaluation shortcut for a cohort study", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  full_grid_prob <- function(y, accrual) {
    mass <- .meta_grid_pmf("plnorm", args, 0, 20, 1, 1, 0.1, accrual)
    n_grid <- floor(20 / 1)
    cell <- floor(y + 0.5)
    frac <- y + 0.5 - cell
    if (cell >= n_grid) {
      return(1)
    }
    grid_cdf <- c(0, cumsum(mass))
    return(grid_cdf[cell + 1] * (1 - frac) + grid_cdf[cell + 2] * frac)
  }
  ys <- c(0.4, 1.5, 3.2, 7.7, 12.3)
  for (accrual in c(0L, 1L)) {
    fast <- vapply(
      ys, .meta_grid_prob, numeric(1),
      dist = "plnorm", args = args,
      cutoff = 20, pwindow = 1, swindow = 1, growth_rate = 0.1,
      accrual = accrual
    )
    full <- vapply(ys, full_grid_prob, numeric(1), accrual = accrual)
    expect_equal(fast, full, tolerance = 1e-12)
  }
})

test_that(".meta_grid_edges only applies to the cohort design", {
  # An accrual grid reweights each cell before normalising, so the ratio of
  # two distribution functions the cohort shortcut uses is wrong for it.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  edges <- .meta_grid_edges(5, "plnorm", args, 0, 20, 1, 1, 0.1)
  cohort <- cumsum(.meta_grid_pmf("plnorm", args, 0, 20, 1, 1, 0.1, 0L))
  accrual <- cumsum(.meta_grid_pmf("plnorm", args, 0, 20, 1, 1, 0.1, 1L))
  expect_equal(edges, c(cohort[5], cohort[6]), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(edges, c(accrual[5], accrual[6]))))
})

test_that(".meta_accrual_weight is linear in the delay at a growth rate of zero", { # nolint: line_length_linter.
  d <- seq(0, 15, by = 0.5)
  linear <- .meta_accrual_weight(d, 16, 0)
  logged <- exp(.meta_log_accrual_weight(d, 16, 0) - log(16))
  expect_equal(linear, logged, tolerance = 1e-12)
  expect_equal(linear, (16 - d) / 16, tolerance = 1e-12)
})

test_that(".meta_implied_density matches a central difference for continuous estimands", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cases <- expand.grid(
    y = c(2.13, 5.07, 9.11), cens_adjusted = c(1, 2, 4),
    trunc_adjusted = c(0L, 1L), trunc_design = c(0L, 1L),
    growth_rate = c(0, 0.1)
  )
  # A primary censored estimand with a growing epidemic has no closed form
  # density, so the implementation is itself a central difference.
  accrual <- cases$trunc_adjusted != 1L & cases$trunc_design == 1L
  cases <- cases[
    !(cases$cens_adjusted != 1 & cases$growth_rate != 0 & !accrual),
  ]
  for (row in seq_len(nrow(cases))) {
    case <- cases[row, ]
    closed <- .meta_implied_density(
      case$y, "plnorm", args, 0, 20, 1, 1, case$trunc_adjusted,
      case$cens_adjusted, case$growth_rate, case$trunc_design
    )
    difference <- .meta_central_difference(
      case$y, "plnorm", args, 0, 20, 1, 1, case$trunc_adjusted,
      case$cens_adjusted, case$growth_rate, case$trunc_design
    )
    expect_equal(closed, difference, tolerance = 1e-4)
  }
})

test_that(".meta_implied_density uses the density of the fitted distribution", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  expect_equal(
    .meta_implied_density(5, "plnorm", args, 0, 20, 1, 1, 1L, 1, 0, 0L),
    stats::dlnorm(5, 1.6, 0.6),
    tolerance = 1e-12
  )
  expect_equal(
    .meta_implied_density(5, "plnorm", args, 0, 20, 1, 1, 0L, 1, 0, 0L),
    stats::dlnorm(5, 1.6, 0.6) / stats::plnorm(20, 1.6, 0.6),
    tolerance = 1e-12
  )
})

test_that(".meta_uniform_pcens_density is the derivative of the primary censored CDF", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  y <- c(0.5, 2.3, 6.1)
  step <- 1e-5
  numeric_density <- (
    .meta_pcens_cdf(y + step, "plnorm", args, 1.7, 0) -
      .meta_pcens_cdf(y - step, "plnorm", args, 1.7, 0)
  ) / (2 * step)
  closed <- vapply(
    y, .meta_uniform_pcens_density, numeric(1),
    dist = "plnorm", args = args, pwindow = 1.7
  )
  expect_equal(closed, numeric_density, tolerance = 1e-6)
})

test_that(".meta_n_quad defaults to the value substituted into the Stan code", {
  chunk <- .stan_chunk(file.path("meta_model", "functions.stan"))
  # The Stan chunk carries a placeholder that epidist_stancode() replaces.
  expect_true(grepl("n_quad_default", chunk, fixed = TRUE))
  expect_identical(.meta_n_quad(), .meta_n_quad_default())
  stanvars <- epidist_stancode(prep_meta_estimates)
  scode <- stanvars[[2]]$scode
  expect_false(grepl("n_quad_default", scode, fixed = TRUE))
  expect_true(
    grepl(paste0(", ", .meta_n_quad(), ","), scode, fixed = TRUE) ||
      grepl(paste0(", ", .meta_n_quad(), "\n"), scode, fixed = TRUE)
  )
})

test_that(".meta_n_quad is configurable and validated", {
  restore <- options(epidist.meta_n_quad = 20L)
  on.exit(options(restore), add = TRUE)
  expect_identical(.meta_n_quad(), 20L)
  stanvars <- epidist_stancode(prep_meta_estimates)
  expect_true(grepl(", 20,", stanvars[[2]]$scode, fixed = TRUE))
  options(epidist.meta_n_quad = 21L)
  expect_error(.meta_n_quad(), "even number")
  options(epidist.meta_n_quad = 1L)
  expect_error(.meta_n_quad())
})

test_that(".meta_n_quad changes the accuracy of the truncated moments", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  fine <- .meta_trunc_moments("plnorm", args, 0, 20)
  restore <- options(epidist.meta_n_quad = 10L)
  on.exit(options(restore), add = TRUE)
  coarse <- .meta_trunc_moments("plnorm", args, 0, 20)
  expect_false(isTRUE(all.equal(fine[["sd"]], coarse[["sd"]])))
  expect_equal(fine[["mean"]], coarse[["mean"]], tolerance = 0.05)
})

test_that(".meta_row_draw_moments caches implied summaries across rows", {
  rm(list = ls(.meta_draws), envir = .meta_draws)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 0,
    obs_type = 2L, cutoff = 40, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0,
    trunc_design = 0L, study_n = 60
  )
  dist_args <- rep(list(args), 3)
  first <- .meta_row_draw_moments(slots, "plnorm", dist_args)
  expect_length(first, 3)
  expect_length(ls(.meta_draws), 1)
  # A second row with the same design and draws reuses the cached summaries.
  second <- .meta_row_draw_moments(
    c(slots[setdiff(names(slots), "obs_type")], list(obs_type = 3L)),
    "plnorm", dist_args
  )
  expect_identical(second, first)
  expect_length(ls(.meta_draws), 1)
  # Quantile rows work on the probability scale and need no summaries.
  quantile_row <- c(
    slots[setdiff(names(slots), "obs_type")], list(obs_type = 6L)
  )
  expect_true(all(vapply(
    .meta_row_draw_moments(quantile_row, "plnorm", dist_args), is.null,
    logical(1)
  )))
  rm(list = ls(.meta_draws), envir = .meta_draws)
})

test_that(".meta_row_draw_moments keys the cache on the quadrature resolution", { # nolint: line_length_linter.
  # Changing the quadrature resolution changes the summaries a design implies,
  # so a cached entry from another resolution must not be reused.
  rm(list = ls(.meta_draws), envir = .meta_draws)
  on.exit(rm(list = ls(.meta_draws), envir = .meta_draws), add = TRUE)
  restore <- options(epidist.meta_n_quad = .meta_n_quad_default())
  on.exit(options(restore), add = TRUE)
  slots <- list(
    lower = 0,
    obs_type = 2L, cutoff = 20, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0,
    trunc_design = 0L, study_n = 60
  )
  dist_args <- list(list(meanlog = 1.6, sdlog = 0.6))
  coarse <- .meta_row_draw_moments(slots, "plnorm", dist_args)[[1]]
  options(epidist.meta_n_quad = 4L)
  fine <- .meta_row_draw_moments(slots, "plnorm", dist_args)[[1]]
  expect_false(isTRUE(all.equal(coarse[["mean"]], fine[["mean"]])))
  expect_identical(fine, .meta_row_moments(slots, "plnorm", dist_args[[1]]))
  expect_length(ls(.meta_draws), 2)
})

test_that("the meta model caches are bounded and stay out of a fitted object", {
  rm(list = ls(.meta_draws), envir = .meta_draws)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  for (cutoff in seq_len(.meta_draw_cache_limit() + 4)) {
    slots <- list(
      lower = 0,
      obs_type = 2L, cutoff = 20 + cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0,
      trunc_design = 0L, study_n = 60
    )
    .meta_row_draw_moments(slots, "plnorm", list(args))
  }
  expect_lte(length(ls(.meta_draws)), .meta_draw_cache_limit())
  # The cache lives in the package namespace rather than the closure's own
  # frame, so using it does not grow what a fitted model would save.
  generator <- suppressMessages(epidist_gen_meta_log_lik(brms::lognormal()))
  before <- ls(environment(generator), all.names = TRUE)
  .meta_row_draw_moments(
    list(
      lower = 0,
      obs_type = 2L, cutoff = 99, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0,
      trunc_design = 0L, study_n = 60
    ),
    "plnorm", rep(list(args), 200)
  )
  expect_identical(ls(environment(generator), all.names = TRUE), before)
  expect_false(".meta_draws" %in% before)
  expect_identical(
    get(".meta_draws", envir = environment(generator)), .meta_draws
  )
  rm(list = ls(.meta_draws), envir = .meta_draws)
})

test_that(".meta_implied_probs matches evaluating each quantile on its own", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  # Includes delays below the grid and beyond the cutoff.
  y <- c(0.05, 0.4, 3.2, 5.5, 9.1, 12.7, 25)
  designs <- expand.grid(
    cens_adjusted = c(0, 1, 2, 3, 4), trunc_adjusted = c(0L, 1L),
    trunc_design = c(0L, 1L), growth_rate = c(0, 0.1)
  )
  for (row in seq_len(nrow(designs))) {
    slots <- c(
      as.list(designs[row, ]),
      list(lower = 0, cutoff = 20, pwindow = 1, swindow = 1)
    )
    batched <- .meta_implied_probs(y, "plnorm", args, slots)
    single <- vapply(
      y, .meta_implied_prob, numeric(1),
      dist = "plnorm", args = args, lower = slots$lower,
      cutoff = slots$cutoff,
      pwindow = slots$pwindow, swindow = slots$swindow,
      trunc_adjusted = slots$trunc_adjusted,
      cens_adjusted = slots$cens_adjusted, growth_rate = slots$growth_rate,
      trunc_design = slots$trunc_design
    )
    expect_equal(batched, single, tolerance = 1e-12)
  }
})

test_that(".meta_implied_moments reduces to the untruncated formulas at a delay_min of zero", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  designs <- expand.grid(
    cens_adjusted = c(0, 1, 2, 3, 4), trunc_adjusted = c(0L, 1L),
    trunc_design = c(0L, 1L), growth_rate = c(0, 0.1)
  )
  for (row in seq_len(nrow(designs))) {
    design <- designs[row, ]
    common <- list(
      dist = "plnorm", args = args, cutoff = 20, pwindow = 1, swindow = 1,
      trunc_adjusted = design$trunc_adjusted,
      cens_adjusted = design$cens_adjusted,
      growth_rate = design$growth_rate, trunc_design = design$trunc_design
    )
    expect_identical(
      do.call(.meta_implied_moments, c(common, list(lower = 0))),
      do.call(.meta_implied_moments, common)
    )
    expect_identical(
      do.call(.meta_implied_prob, c(list(y = 5), common, list(lower = 0))),
      do.call(.meta_implied_prob, c(list(y = 5), common))
    )
    expect_identical(
      do.call(.meta_implied_density, c(list(y = 5), common, list(lower = 0))),
      do.call(.meta_implied_density, c(list(y = 5), common))
    )
  }
})

test_that(".meta_implied_moments conditions a continuous estimand on delay_min", { # nolint: line_length_linter.
  set.seed(203)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 60
  moments <- .meta_implied_moments(
    "plnorm", args,
    lower = 3, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0
  )
  delay <- rlnorm(2e6, args$meanlog, args$sdlog)
  obs <- delay[delay > 3 & delay <= cutoff]
  expect_equal(moments[["mean"]], mean(obs), tolerance = 0.005)
  expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.01)
})

test_that(".meta_implied_prob conditions the naive grid on delay_min", {
  set.seed(204)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  cutoff <- 25
  n_sim <- 2e6
  ptime <- runif(n_sim, 0, 1)
  delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
  daily <- floor(ptime + delay)
  observed <- daily[daily >= 4 & daily + 1 <= cutoff]
  prob <- .meta_implied_prob(
    8, "plnorm", args,
    lower = 4, cutoff = cutoff, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
  )
  # The continuity correction interpolates through the cell mid points, so
  # the implied probability at a grid value is the average of the two sides
  # of the jump there.
  expect_equal(
    prob, mean(c(mean(observed <= 7), mean(observed <= 8))),
    tolerance = 0.01
  )
  expect_identical(
    .meta_implied_prob(
      3.4, "plnorm", args,
      lower = 4, cutoff = cutoff, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = 0
    ),
    0
  )
})

test_that(".meta_node_quantile inverts the implied distribution function", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  for (cens_adjusted in c(0, 1, 2, 3, 4)) {
    slots <- list(
      lower = 0, cutoff = 60, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = cens_adjusted, growth_rate = 0,
      trunc_design = 0L
    )
    nodes <- .meta_implied_nodes("plnorm", args, slots)
    # A discrete estimand is inverted on its own grid, so its round trip is
    # exact. A continuous one is only as accurate as the interpolation.
    for (p in c(0.1, 0.5, 0.9)) {
      value <- .meta_node_quantile(nodes, p)
      round_trip <- .meta_implied_probs(value, "plnorm", args, slots)
      if (cens_adjusted %in% c(0, 3)) {
        expect_equal(round_trip, p, tolerance = 1e-12)
      } else {
        expect_lt(abs(round_trip - p), 5e-3)
      }
    }
  }
})

test_that(".meta_node_quantile conditions on delay_min", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 4, cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  median <- .meta_node_quantile(nodes, 0.5)
  # The median of the delay distribution conditioned on exceeding four.
  target <- stats::qlnorm(
    stats::plnorm(4, 1.6, 0.6) + 0.5 * (1 - stats::plnorm(4, 1.6, 0.6)),
    1.6, 0.6
  )
  expect_equal(median, target, tolerance = 0.01)
  expect_gt(median, 4)
})

test_that(".meta_multi_normal_ll matches the multivariate normal density", {
  covariance <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  y <- c(6.2, 3.1)
  implied <- c(6.0, 3.4)
  quadratic <- t(y - implied) %*% solve(covariance) %*% (y - implied)
  expected <- as.numeric(
    -log(2 * pi) - 0.5 * log(det(covariance)) - 0.5 * quadratic
  )
  expect_equal(
    .meta_multi_normal_ll(y, implied, t(chol(covariance))), expected,
    tolerance = 1e-12
  )
  expect_identical(
    .meta_multi_normal_ll(y, c(Inf, 3.4), t(chol(covariance))), -Inf
  )
})

test_that("as_epidist_meta_model builds a multivariate normal row from a reported covariance", { # nolint: line_length_linter.
  covariance <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  estimates <- suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = c("A", "A"), type = c("mean", "sd"), value = c(6.2, 3.1),
      relative_obs_time = c(30, 30), trunc_adjusted = c(FALSE, FALSE),
      cens_adjusted = c(0, 0), n = c(NA, NA), stringsAsFactors = FALSE
    ),
    vcov = list(A = covariance)
  ))
  meta <- suppressMessages(as_epidist_meta_model(estimates))
  expect_identical(meta$obs_type, 7L)
  expect_identical(meta$group_len, 2L)
  expect_identical(.meta_chol(meta), as.numeric(t(chol(covariance))))
  members <- .meta_members(meta)
  expect_identical(members$type, c(1L, 2L))
  expect_identical(members$value, c(6.2, 3.1))
})

test_that("as_epidist_meta_model passes delay_min through from linelist data", {
  full <- suppressMessages(as_epidist_linelist_data(
    sierra_leone_ebola_data,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ))
  expect_true(all(suppressMessages(as_epidist_meta_model(full))$delay_min == 0))
  # A study that only counted delays above a minimum cannot also hold rows
  # below it, exactly as for the marginal model.
  expect_error(
    suppressMessages(as_epidist_meta_model(full, delay_min = 2)),
    "delay_min"
  )
  kept <- full[full$stime_lwr - full$ptime_lwr >= 2, , drop = FALSE]
  meta <- suppressMessages(as_epidist_meta_model(kept, delay_min = 2))
  expect_true(all(meta$delay_min == 2))
  expect_true(all(meta$delay_lwr >= 2))
})

test_that("assert_epidist.epidist_meta_model rejects an out of range delay_min", { # nolint: line_length_linter.
  meta <- suppressMessages(as_epidist_meta_model(estimates = sim_estimates))
  meta$delay_min <- meta$relative_obs_time
  expect_error(assert_epidist(meta), "must be below")
})

test_that("the meta model individual level slots line up with the marginal model", { # nolint: line_length_linter.
  # The marginal model generators read the first five vreal slots by
  # position, so the two layouts must agree on them.
  data <- suppressMessages(as_epidist_linelist_data(
    sierra_leone_ebola_data,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ))
  marginal <- suppressMessages(as_epidist_marginal_model(data))
  meta <- suppressMessages(as_epidist_meta_model(data))
  marginal_form <- as_string_formula(suppressWarnings(epidist_formula(
    marginal, epidist_family(marginal),
    formula = bf(mu ~ 1)
  ))$formula)
  meta_form <- as_string_formula(suppressWarnings(epidist_formula(
    meta, epidist_family(meta),
    formula = bf(mu ~ 1)
  ))$formula)
  slots <- function(form) {
    inside <- sub("^.*vreal\\(", "", form)
    return(trimws(strsplit(sub("\\).*$", "", inside), ",", fixed = TRUE)[[1]]))
  }
  expect_identical(slots(meta_form)[1:5], slots(marginal_form)[1:5])
})

test_that("as_epidist_meta_model errors when summary estimates are supplied twice", { # nolint: line_length_linter.
  expect_error(
    as_epidist_meta_model(sim_estimates, estimates = sim_estimates),
    "supplied twice"
  )
})

test_that("assert_epidist.epidist_meta_model checks the individual level rows", { # nolint: line_length_linter.
  wide <- prep_meta_individual
  wide$delay_upr[1] <- wide$delay_upr[1] + 1
  expect_error(assert_epidist(wide), "must equal")
  early <- prep_meta_individual
  early$relative_obs_time[1] <- early$delay_upr[1] - 0.5
  expect_error(
    assert_epidist(early), "`relative_obs_time` must be greater"
  )
  truncated <- prep_meta_individual
  truncated$delay_min[1] <- truncated$delay_lwr[1] + 1
  expect_error(assert_epidist(truncated), "`delay_lwr` must be greater")
})

test_that("assert_epidist.epidist_meta_model checks the summary rows", {
  small <- prep_meta_estimates
  small$study_n <- 1L
  expect_error(assert_epidist(small), "at least 2")
  unbounded <- prep_meta_estimates
  unbounded$relative_obs_time[1] <- Inf
  expect_error(assert_epidist(unbounded), "finite grid cutoff")
  narrow <- prep_meta_estimates
  narrow$swindow[1] <- narrow$relative_obs_time[1] + 1
  expect_error(assert_epidist(narrow), "grid cutoff for summary rows")
  quantile_row <- which(prep_meta_estimates$obs_type == 6L)[1]
  flat <- prep_meta_estimates
  flat$quantile_p[quantile_row] <- 0
  expect_error(assert_epidist(flat), "strictly between 0 and 1")
})

test_that("assert_epidist.epidist_meta_model checks a covariance matrix row", {
  covariance <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  estimates <- suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = c("A", "A"), type = c("mean", "sd"), value = c(6.2, 3.1),
      relative_obs_time = c(30, 30), trunc_adjusted = c(FALSE, FALSE),
      cens_adjusted = c(0, 0), n = c(NA, NA), stringsAsFactors = FALSE
    ),
    vcov = list(A = covariance)
  ))
  meta <- suppressMessages(as_epidist_meta_model(estimates))
  empty <- meta
  empty$group_len <- 0L
  expect_error(assert_epidist(empty), "at least one grouped summary member")
  # The factor of a two by two matrix has four entries, so a row starting at
  # the second of them runs off the end of the flat vector passed to Stan.
  overrun <- meta
  overrun$chol_start <- 2L
  expect_error(assert_epidist(overrun), "full Cholesky factor")
})

test_that("assert_epidist.epidist_meta_model checks a joint quantile row", {
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("quantile", "quantile"), value = c(4, 8),
    p = c(0.25, 0.75), n = 100, relative_obs_time = 30,
    trunc_adjusted = FALSE, cens_adjusted = 0, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, 6L)
  members <- .meta_members(meta)
  expect_identical(members$count, c(25L, 75L))
  none <- meta
  none$group_len <- 0L
  expect_error(assert_epidist(none), "at least one grouped summary member")
  falling <- members
  falling$value <- c(8, 4)
  expect_error(
    assert_epidist(.meta_set_members(meta, falling)), "strictly increasing"
  )
  dropping <- members
  dropping$count <- c(75L, 25L)
  expect_error(
    assert_epidist(.meta_set_members(meta, dropping)), "cumulative counts"
  )
  beyond <- members
  beyond$count <- c(25L, 500L)
  expect_error(
    assert_epidist(.meta_set_members(meta, beyond)), "cumulative counts"
  )
})

test_that(".meta_ddist pairs each distribution function with its density", {
  expect_identical(.meta_ddist("plnorm"), stats::dlnorm)
  expect_identical(.meta_ddist("pgamma"), stats::dgamma)
  expect_identical(.meta_ddist("pweibull"), stats::dweibull)
  # Any other name is taken from stats by dropping the leading p.
  expect_identical(.meta_ddist("pnorm"), stats::dnorm)
})

test_that(".meta_log_accrual_weight matches the follow up integral", {
  d <- c(0, 2, 5, 9)
  window <- 12
  for (rate in c(0.25, -0.25)) {
    expect_equal(
      .meta_log_accrual_weight(d, window, rate),
      log(expm1(rate * (window - d)) / rate),
      tolerance = 1e-10
    )
  }
})

test_that(".meta_accrual_weight is zero once the collection window closes", {
  # A delay at or beyond the window has no follow up, so every weight is zero
  # and the relative weighting has nothing to divide by.
  expect_identical(.meta_accrual_weight(c(12, 15), 12, 0), c(0, 0))
  expect_true(all(.meta_accrual_weight(c(0, 6, 12), 12, 0) == c(1, 0.5, 0)))
})

test_that(".meta_accrual_reweight returns the input when the mass underflows", { # nolint: line_length_linter.
  # A distribution function that is flat over the quadrature holds no mass to
  # reweight, so the guard returns it rather than dividing by zero.
  flat <- rep(0, 11)
  expect_identical(.meta_accrual_reweight(flat, 0, 10, 0), flat)
})

test_that(".meta_grid_pmf errors when delay_min leaves no grid cells", {
  expect_error(
    .meta_grid_pmf(
      "plnorm", list(meanlog = 1.6, sdlog = 0.6),
      lower = 10, cutoff = 10, pwindow = 1, swindow = 1, growth_rate = 0
    ),
    "holds no cells"
  )
})

test_that("an accrual grid guards against an underflowing mass", {
  args <- list(meanlog = 100, sdlog = 0.1)
  mass <- .meta_grid_pmf("plnorm", args, 0, 5, 1, 1, 0.1, 1L)
  expect_length(mass, 5)
  expect_true(all(is.na(mass)))
  expect_identical(
    .meta_grid_prob(3, "plnorm", args, 0, 5, 1, 1, 0.1, 1L), Inf
  )
  expect_identical(
    .meta_implied_density(3, "plnorm", args, 0, 5, 1, 1, 0L, 0L, 0.1, 1L), Inf
  )
  slots <- list(
    lower = 0, cutoff = 5, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 0L, growth_rate = 0.1, trunc_design = 1L
  )
  expect_true(
    all(is.infinite(.meta_implied_probs(c(2, 3), "plnorm", args, slots)))
  )
})

test_that(".meta_grid_probs pins delays outside the grid and guards underflow", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  expect_identical(
    .meta_grid_probs(c(-1, 25), "plnorm", args, 0, 20, 1, 1, 0), c(0, 1)
  )
  under <- .meta_grid_probs(
    c(2, 3), "plnorm", list(meanlog = 100, sdlog = 0.1), 0, 5, 1, 1, 0
  )
  expect_true(all(is.infinite(under)))
})

test_that(".meta_implied_moments rejects an unsupported distribution", {
  expect_error(
    .meta_implied_moments(
      "pnorm", list(mean = 5, sd = 2),
      cutoff = 20, pwindow = 1, swindow = 1,
      trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
    ),
    "not supported for"
  )
})

test_that("the accrual estimand is pinned outside the delays a study saw", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  expect_identical(
    .meta_accrual_prob(20, "plnorm", args, 2, 20, 1, 1L, 0.1), 1
  )
  expect_identical(
    .meta_accrual_prob(2, "plnorm", args, 2, 20, 1, 1L, 0.1), 0
  )
  expect_identical(
    .meta_accrual_density(20, "plnorm", args, 2, 20, 1, 1L, 0.1), 0
  )
  expect_identical(
    .meta_accrual_density(2, "plnorm", args, 2, 20, 1, 1L, 0.1), 0
  )
  # Between the two the distribution function is increasing, so the density
  # there is positive.
  expect_gt(.meta_accrual_density(6, "plnorm", args, 2, 20, 1, 1L, 0.1), 0)
})

test_that(".meta_node_quantile returns NA when the implied nodes underflow", {
  args <- list(meanlog = 100, sdlog = 0.1)
  slots <- list(
    lower = 0, cutoff = 5, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  expect_true(all(is.na(nodes$values)))
  expect_identical(.meta_node_quantile(nodes, 0.5), NA_real_)
  # A quantile the model cannot imply rejects the draw rather than returning
  # a NaN log likelihood.
  covariance <- diag(2)
  member_slots <- c(slots, list(
    obs_type = 7L, group_type = c(3L, 3L), group_p = c(0.25, 0.75),
    group_value = c(4, 8), group_chol = t(chol(covariance))
  ))
  expect_identical(
    .meta_row_log_lik(member_slots, "plnorm", args), -Inf
  )
})

test_that(".meta_node_quantile pins probabilities outside the implied range", { # nolint: line_length_linter.
  nodes <- list(values = c(0.2, 0.5, 0.7, 0.9), origin = 1, spacing = 2)
  # Below the first node the estimand has no support, and at or above the last
  # it has run out, so both are pinned to the ends of the grid.
  expect_identical(.meta_node_quantile(nodes, 0.1), 1)
  expect_identical(.meta_node_quantile(nodes, 0.95), 7)
  # In between the delay is interpolated linearly across the step it falls in.
  expect_equal(.meta_node_quantile(nodes, 0.35), 2, tolerance = 1e-12)
})

test_that("a meta model without grouped members falls back to an empty table", { # nolint: line_length_linter.
  bare <- prep_meta_individual
  attr(bare, "meta_members") <- NULL
  attr(bare, "meta_chol") <- NULL
  expect_identical(nrow(.meta_members(bare)), 0L)
  expect_identical(.meta_chol(bare), numeric(0))
  expect_no_error(assert_epidist(bare))
})

test_that(".meta_implied_prob rejects a draw holding no mass above delay_min", { # nolint: line_length_linter.
  # A distribution far below the smallest delay counted leaves nothing to
  # condition on, so the draw is rejected rather than dividing by zero.
  args <- list(meanlog = -100, sdlog = 0.1)
  expect_identical(
    .meta_implied_prob(6, "plnorm", args, 4, 30, 1, 1, 1L, 1L, 0), Inf
  )
})

test_that("the implied estimand holds no mass outside the delays a study saw", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  # Below the smallest delay the study counted.
  expect_identical(
    .meta_implied_prob(2, "plnorm", args, 4, 30, 1, 1, 1L, 1L, 0), 0
  )
  expect_identical(
    .meta_implied_density(2, "plnorm", args, 4, 30, 1, 1, 1L, 1L, 0), 0
  )
  expect_identical(
    .meta_implied_density(0, "plnorm", args, 4, 30, 1, 1, 0L, 0L, 0), 0
  )
  # Beyond the observation time of a study that did not adjust for right
  # truncation.
  expect_identical(
    .meta_implied_density(35, "plnorm", args, 4, 30, 1, 1, 0L, 1L, 0), 0
  )
  expect_identical(
    .meta_implied_density(40, "plnorm", args, 0, 30, 1, 1, 0L, 0L, 0), 0
  )
})

test_that(".meta_implied_nodes reweights a continuous estimand for accrual", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 0, cutoff = 20, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 1L, growth_rate = 0.1, trunc_design = 1L
  )
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  expect_identical(nodes$values[1], 0)
  expect_equal(nodes$values[length(nodes$values)], 1, tolerance = 1e-12)
  # An accrual design sees fewer of the long delays than a cohort followed for
  # the same time, so its distribution function sits above the cohort one.
  cohort <- .meta_implied_nodes(
    "plnorm", args, modifyList(slots, list(trunc_design = 0L))
  )
  inner <- seq(2, length(nodes$values) - 1)
  expect_true(all(nodes$values[inner] > cohort$values[inner]))
})

test_that(".meta_quantile_set_ll rejects a draw whose grid mass underflows", {
  args <- list(meanlog = 100, sdlog = 0.1)
  slots <- list(
    lower = 0, cutoff = 5, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 0L, growth_rate = 0, trunc_design = 0L
  )
  expect_identical(
    .meta_quantile_set_ll(c(2, 3), c(20, 60), 100, "plnorm", args, slots),
    -Inf
  )
})

test_that(".meta_summary_terms predicts the first member of a covariance row", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.6)
  covariance <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  slots <- list(
    lower = 0, cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L, obs_type = 7L,
    group_type = c(1L, 2L), group_p = c(0, 0), group_value = c(6.2, 3.1),
    group_chol = t(chol(covariance))
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  moments <- .meta_row_moments(slots, "plnorm", args)
  expect_identical(terms[["observed"]], 6.2)
  expect_identical(terms[["implied"]], unname(moments[["mean"]]))
  # The marginal standard deviation of the first member is the leading entry
  # of the Cholesky factor of the reported covariance matrix.
  expect_equal(terms[["se"]], sqrt(covariance[1, 1]), tolerance = 1e-12)
})

test_that("the meta model returns NA for summary rows of an unsupported family", { # nolint: line_length_linter.
  prep <- list(data = list(vint1 = 2L), ndraws = 4L)
  log_lik <- NULL
  messages <- capture_messages({
    log_lik <- epidist_gen_meta_log_lik(brms::brmsfamily("gaussian"))
  })
  expect_true(any(grepl("not supported in R", messages, fixed = TRUE)))
  expect_identical(log_lik(1, prep), rep(NA_real_, 4))
  predict_fn <- suppressMessages(
    epidist_gen_meta_predict(brms::brmsfamily("gaussian"))
  )
  expect_identical(predict_fn(1, prep), as.matrix(rep(NA_real_, 4)))
})
