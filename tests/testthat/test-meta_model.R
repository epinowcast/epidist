# fmt: skip file

# Modifying a meta model object re-checks it and drops the class with a
# warning when the change breaks it, which the epidist_data tests cover. The
# assertion message is checked on the reclassed object.
expect_meta_assert_error <- function(data, pattern) {
  expect_false(is_epidist_meta_model(data))
  return(expect_error(
    assert_epidist(.new_epidist_data(data, "epidist_meta_model")), pattern
  ))
}
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

test_that("meta model objects carry the shared epidist_data class", {
  expect_true(is_epidist_data(prep_meta_individual))
  expect_true(is_epidist_data(prep_meta_estimates))
  expect_true(is_epidist_data(prep_meta_obs))
  # The class order follows the marginal model: the specific classes first,
  # then the shared class once, then the underlying data frame classes.
  expect_identical(
    class(prep_meta_obs)[1:2], c("epidist_meta_model", "epidist_data")
  )
  expect_s3_class(prep_meta_obs, "tbl_df")
  expect_identical(
    match("epidist_data", class(prep_meta_obs)),
    length(.epidist_classes(prep_meta_obs)) + 1L
  )
  expect_identical(
    match("epidist_data", class(prep_marginal_obs)),
    length(.epidist_classes(prep_marginal_obs)) + 1L
  )
  expect_identical(.primary_dist(prep_meta_obs), "uniform")
  expect_false(is_epidist_data(.drop_epidist_class(prep_meta_obs)))
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

test_that("as_epidist_meta_model does not repeat the estimates data checks", { # nolint: line_length_linter.
  # The advisory checks run once, when the estimates are built, and not
  # again when the finished object is passed on to the meta model.
  heavy <- data.frame(
    study = "A", type = c("mean", "sd"),
    value = c(exp(2.1), exp(2.1) * sqrt(expm1(1))), n = 100,
    relative_obs_time = Inf, trunc_adjusted = TRUE, cens_adjusted = 1,
    pwindow = 1, swindow = 1, max_delay = 200, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(heavy))
  expect_true(any(grepl("relative standard error", msgs, fixed = TRUE)))
  estimates <- suppressMessages(as_epidist_estimates_data(heavy))
  expect_silent(as_epidist_meta_model(estimates = estimates))
  expect_silent(as_epidist_meta_model(estimates))
  expect_silent(as_epidist_meta_model(sim_obs, estimates = estimates))
  # Combining finished objects checks the combined object once, so each
  # check and the pointer to the documentation print once.
  msgs <- capture_messages(
    as_epidist_estimates_data(list(estimates, estimates))
  )
  expect_identical(sum(grepl("relative standard error", msgs)), 1L)
  expect_identical(sum(grepl("Checks section", msgs)), 1L)
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
  expect_error(assert_epidist(suppressWarnings(prep_meta_obs[, 1])))
  bad <- prep_meta_obs
  expect_warning(
    {
      bad$obs_type[1] <- 7L
    },
    "Dropping"
  )
  expect_meta_assert_error(bad, NULL)
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
      "trunc_design, group_start, group_len, chol_start, n_quad)"
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
    moments[["kurtosis"]], simulated[["kurtosis"]],
    tolerance = 0.03
  )
  expect_equal(
    moments[["skewness"]], simulated[["skewness"]],
    tolerance = 0.03
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
    moments[["sd"]], expected_mean * sqrt(expm1(s2)),
    tolerance = 1e-8
  )
  expect_equal(
    moments[["kurtosis"]],
    exp(4 * s2) + 2 * exp(3 * s2) + 3 * exp(2 * s2) - 3,
    tolerance = 1e-8
  )
  expect_equal(
    moments[["skewness"]], (exp(s2) + 2) * sqrt(expm1(s2)),
    tolerance = 1e-8
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
    moments[["kurtosis"]], simulated[["kurtosis"]],
    tolerance = 0.02
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
  # Code 4 summarises tau + U_p - pwindow / 2. A study that only counted
  # reported delays of at least delay_min dropped the records whose
  # midpointed delay fell below it, so the base estimand is left truncated at
  # delay_min + pwindow / 2 rather than at delay_min.
  set.seed(115)
  args <- list(meanlog = 1.8, sdlog = 0.5)
  n_sim <- 1e6
  designs <- expand.grid(
    pwindow = c(1, 3), obs_time = c(20, Inf), delay_min = c(0, 3)
  )
  for (row in seq_len(nrow(designs))) {
    pwindow <- designs$pwindow[row]
    obs_time <- designs$obs_time[row]
    delay_min <- designs$delay_min[row]
    trunc_adjusted <- as.integer(is.infinite(obs_time))
    cutoff <- if (is.infinite(obs_time)) 80 else obs_time
    moments <- .meta_implied_moments(
      "plnorm", args,
      lower = delay_min, cutoff = cutoff, pwindow = pwindow, swindow = 1,
      trunc_adjusted = trunc_adjusted, cens_adjusted = 4L, growth_rate = 0
    )
    raw <- stats::runif(n_sim, 0, pwindow) +
      stats::rlnorm(n_sim, args$meanlog, args$sdlog)
    observed <- raw[raw <= obs_time] - pwindow / 2
    observed <- observed[observed >= delay_min]
    expect_equal(moments[["mean"]], mean(observed), tolerance = 0.01)
    expect_equal(moments[["sd"]], stats::sd(observed), tolerance = 0.01)
    # The implied distribution function must agree with the same simulation.
    reported <- stats::quantile(observed, c(0.25, 0.5, 0.9), names = FALSE)
    implied <- vapply(
      reported,
      .meta_implied_prob,
      numeric(1),
      dist = "plnorm", args = args, lower = delay_min, cutoff = cutoff,
      pwindow = pwindow, swindow = 1, trunc_adjusted = trunc_adjusted,
      cens_adjusted = 4L, growth_rate = 0
    )
    expect_equal(implied, c(0.25, 0.5, 0.9), tolerance = 0.01)
    if (delay_min > 0) {
      # Nothing the study reported can sit below the smallest delay it
      # counted.
      expect_identical(.meta_implied_prob(
        delay_min, "plnorm", args,
        lower = delay_min, cutoff = cutoff, pwindow = pwindow, swindow = 1,
        trunc_adjusted = trunc_adjusted, cens_adjusted = 4L, growth_rate = 0
      ), 0)
    }
  }
})

test_that(".meta_implied_moments recovers a left truncated study that midpoints a wide secondary window", { # nolint: line_length_linter.
  # Code 3 reports (j + 1 / 2) * swindow for grid cell j, so a study that
  # only counted reported delays of at least delay_min kept the cells from
  # ceiling(delay_min / swindow - 1 / 2), which differs from the untruncated
  # grid rule whenever delay_min / swindow is not an integer.
  set.seed(116)
  args <- list(meanlog = 1.6, sdlog = 0.6)
  swindow <- 2
  delay_min <- 3
  cutoff <- 40
  n_sim <- 1e6
  raw <- stats::runif(n_sim, 0, 1) +
    stats::rlnorm(n_sim, args$meanlog, args$sdlog)
  grid <- swindow * floor(raw / swindow)
  reported <- grid[grid + swindow <= cutoff] + swindow / 2
  reported <- reported[reported >= delay_min]
  shared <- list(
    dist = "plnorm", args = args, lower = delay_min, cutoff = cutoff,
    pwindow = 1, swindow = swindow, trunc_adjusted = 0L, cens_adjusted = 3L,
    growth_rate = 0
  )
  moments <- do.call(.meta_implied_moments, shared)
  expect_equal(moments[["mean"]], mean(reported), tolerance = 0.01)
  expect_equal(moments[["sd"]], stats::sd(reported), tolerance = 0.01)
  # Between two support points the continuity corrected distribution function
  # equals the empirical one at the lower of them.
  for (y in c(4, 6, 8, 12)) {
    expect_equal(
      do.call(.meta_implied_prob, c(list(y = y), shared)),
      mean(reported <= y - 1),
      tolerance = 0.01
    )
  }
  # The cell whose reported value equals delay_min is kept, not dropped.
  expect_gt(do.call(.meta_implied_density, c(list(y = delay_min), shared)), 0)
  slots <- c(shared[-(1:2)], list(trunc_design = 0L))
  expect_equal(
    .meta_implied_probs(c(4, 8), "plnorm", args, slots),
    c(mean(reported <= 3), mean(reported <= 7)),
    tolerance = 0.01
  )
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  expect_identical(nodes$origin, delay_min - swindow / 2)
  # The nodes start at the kept cell, so a quantile read off them and fed
  # back through the distribution function round trips exactly.
  median <- .meta_node_quantile(nodes, 0.5)
  expect_gt(median, delay_min)
  expect_equal(
    .meta_implied_probs(median, "plnorm", args, slots), 0.5,
    tolerance = 1e-12
  )
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
        midpoint[["mean"]], uniform[["mean"]] - pwindow / 2,
        tolerance = 1e-10
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
    list(
      dist = "plnorm", args = list(meanlog = 1.6, sdlog = 0.6),
      cutoff = 400, accrual = 0L
    ),
    list(
      dist = "plnorm", args = list(meanlog = 1, sdlog = 0.4),
      cutoff = 100, accrual = 0L
    ),
    list(
      dist = "plnorm", args = list(meanlog = 1.6, sdlog = 0.6),
      cutoff = 400, accrual = 1L
    ),
    list(
      dist = "pgamma", args = list(shape = 4, scale = 1),
      cutoff = 50, accrual = 0L
    )
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

test_that(".meta_row_log_lik rejects a draw whose implied moments overflow", {
  # A very wide lognormal overflows the analytic kurtosis to NaN. The joint
  # likelihood of a mean and standard deviation pair already rejects such a
  # draw, and an ungrouped row must do the same rather than return NaN.
  args <- list(meanlog = 1.6, sdlog = 15)
  expect_false(all(is.finite(.meta_continuous_moments("plnorm", args))))
  slots <- list(
    lower = 0, obs_type = 3L, study_n = 100L, trunc_adjusted = 1L,
    cens_adjusted = 1L, cutoff = 60, pwindow = 1, swindow = 1, value = 3.6,
    report_se = 0, quantile_p = 0, growth_rate = 0, trunc_design = 0L,
    group_value = c(7.5, 3.6)
  )
  expect_identical(.meta_row_log_lik(slots, "plnorm", args), -Inf)
  slots$obs_type <- 2L
  expect_identical(.meta_row_log_lik(slots, "plnorm", args), -Inf)
  slots$report_se <- 0.5
  expect_identical(.meta_row_log_lik(slots, "plnorm", args), -Inf)
  slots$obs_type <- 5L
  expect_identical(.meta_row_log_lik(slots, "plnorm", args), -Inf)
  # A finite draw is unaffected.
  finite <- list(meanlog = 1.6, sdlog = 0.6)
  for (obs_type in c(2L, 3L, 5L)) {
    slots$obs_type <- obs_type
    expect_true(is.finite(.meta_row_log_lik(slots, "plnorm", finite)))
  }
})

test_that("the distribution functions sever nodes deep in the lower tail", {
  # primarycensored's Stan lcdf has a finite value with a NaN gradient deep
  # in the lower tail of a narrow delay, where every grid and quadrature path
  # evaluates it. Both implementations therefore treat a node whose plain log
  # distribution function is below -100 as holding no mass, before the
  # primary censored function is called. Matches meta_family_pcens_lcdf()
  # and meta_family_dist_prob() in Stan.
  args <- list(meanlog = 1.8, sdlog = 0.05)
  deep <- 2
  shallow <- 4
  expect_lt(stats::plnorm(deep, 1.8, 0.05, log.p = TRUE), -100)
  expect_gt(stats::plnorm(shallow, 1.8, 0.05, log.p = TRUE), -100)
  expect_gt(stats::plnorm(deep, 1.8, 0.05), 0)
  expect_identical(.meta_pcens_cdf(deep, "plnorm", args, 1, 0), 0)
  expect_gt(.meta_pcens_cdf(shallow, "plnorm", args, 1, 0), 0)
  expect_identical(.meta_pcens_cdf(deep, "plnorm", args, 1, 0.1), 0)
  expect_identical(.meta_dist_cdf(deep, "plnorm", args), 0)
  expect_identical(
    .meta_dist_cdf(shallow, "plnorm", args), stats::plnorm(shallow, 1.8, 0.05)
  )
  expect_identical(.meta_dist_cdf(c(-1, 0), "plnorm", args), c(0, 0))
  # The cut is decided from a closed form bound on the parameters for every
  # family, so that Stan never evaluates the distribution function there.
  expect_true(.meta_deep_tail(deep, "plnorm", args))
  expect_false(.meta_deep_tail(shallow, "plnorm", args))
  gamma_args <- list(shape = 3, scale = 2)
  expect_true(.meta_deep_tail(1e-15, "pgamma", gamma_args))
  expect_false(.meta_deep_tail(1, "pgamma", gamma_args))
  expect_identical(.meta_dist_cdf(1e-15, "pgamma", gamma_args), 0)
  weibull_args <- list(shape = 2, scale = 7)
  expect_true(.meta_deep_tail(1e-25, "pweibull", weibull_args))
  expect_false(.meta_deep_tail(1, "pweibull", weibull_args))
  expect_identical(.meta_dist_cdf(1e-25, "pweibull", weibull_args), 0)
  # The bounds only sever what the cut itself would.
  for (family in list(
    list("plnorm", args), list("pgamma", gamma_args),
    list("pweibull", weibull_args)
  )) {
    grid <- exp(seq(-40, 3, by = 0.05))
    bound <- .meta_deep_tail(grid, family[[1]], family[[2]])
    cdf <- do.call(.pdist(family[[1]]), c(list(q = grid), family[[2]]))
    expect_true(all(cdf[bound] < exp(.meta_log_cdf_floor())))
  }
  # The severed mass is below anything a moment or a probability resolves.
  full <- .meta_trunc_moments("plnorm", args, 0, 12)
  expect_equal(
    full[["mean"]], exp(1.8 + 0.05^2 / 2),
    tolerance = 1e-8
  )
})

test_that(".meta_continuous_moments rejects a draw whose moments overflow", {
  # An overflowing analytic moment is returned as the failure vector, so the
  # log likelihood is -Inf and the posterior predictive has an infinite rather
  # than a NaN standard error. Matches the reject in meta_family_moments().
  wide <- .meta_continuous_moments("plnorm", list(meanlog = 1.6, sdlog = 15))
  expect_identical(wide, .meta_moment_failure())
  narrow_shape <- .meta_continuous_moments(
    "pweibull", list(shape = 0.01, scale = 5)
  )
  expect_identical(narrow_shape, .meta_moment_failure())
  slots <- list(
    lower = 0, obs_type = 3L, study_n = 100L, trunc_adjusted = 1L,
    cens_adjusted = 1L, cutoff = 60, pwindow = 1, swindow = 1, value = 3.6,
    report_se = 0, quantile_p = 0, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", list(meanlog = 1.6, sdlog = 15))
  expect_false(anyNA(terms))
  expect_identical(unname(terms[c("implied", "se")]), c(Inf, Inf))
  slots$report_se <- 0.5
  terms <- .meta_summary_terms(slots, "plnorm", list(meanlog = 1.6, sdlog = 15))
  expect_identical(unname(terms[c("implied", "se")]), c(Inf, Inf))
  # A finite draw is unaffected.
  finite <- .meta_continuous_moments("plnorm", list(meanlog = 1.6, sdlog = 0.6))
  expect_true(all(is.finite(finite)))
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
    moments[["kurtosis"]], simulated[["kurtosis"]],
    tolerance = 0.05
  )
  expect_equal(
    moments[["skewness"]], simulated[["skewness"]],
    tolerance = 0.02
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

test_that(".meta_implied_moments matches Monte Carlo accrual truncation with unequal windows", { # nolint: line_length_linter.
  # The follow up available to a case depends on the primary window it fell
  # in, so within a reporting cell wider than the primary window the accrual
  # weight steps down at every multiple of pwindow, and a cell narrower than
  # the primary window shares one weight with its neighbours. A weekly
  # reported secondary date against a daily primary date at an outbreak
  # growth rate is where the lower edge weight fails worst.
  set.seed(123)
  mean <- 4.6
  sd <- 2.4
  var_log <- log1p((sd / mean)^2)
  args <- list(meanlog = log(mean) - var_log / 2, sdlog = sqrt(var_log))
  window <- 28
  n_sim <- 2e6
  designs <- expand.grid(
    growth_rate = c(0.05, 0.2), pwindow = c(1, 7), swindow = c(7, 1)
  )
  designs <- designs[designs$pwindow != designs$swindow, ]
  for (row in seq_len(nrow(designs))) {
    growth_rate <- designs$growth_rate[row]
    pwindow <- designs$pwindow[row]
    swindow <- designs$swindow[row]
    moments <- .meta_implied_moments(
      "plnorm", args,
      cutoff = window, pwindow = pwindow, swindow = swindow,
      trunc_adjusted = 0L, cens_adjusted = 0L, growth_rate = growth_rate,
      trunc_design = 1L
    )
    ptime <- sim_accrual_ptime(n_sim, window, growth_rate)
    delay <- rlnorm(n_sim, args$meanlog, args$sdlog)
    keep <- ptime + delay <= window
    ptime <- ptime[keep]
    delay <- delay[keep]
    if (pwindow < swindow) {
      # Daily date differencing, then reported by week.
      daily <- floor(ptime + delay) - floor(ptime)
      obs <- swindow * floor(daily / swindow)
    } else {
      # A daily secondary date against the start of the primary week.
      obs <- floor(ptime + delay) - pwindow * floor(ptime / pwindow)
    }
    expect_equal(moments[["mean"]], mean(obs), tolerance = 0.02)
    expect_equal(moments[["sd"]], stats::sd(obs), tolerance = 0.02)
  }
})

test_that(".meta_grid_pmf reduces to the lower edge accrual weight for equal windows", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  boundary <- seq(0, 20)
  cdf <- .meta_pcens_cdf(boundary, "plnorm", args, 1, 0.1)
  expected <- pmax(diff(cdf), 0) *
    .meta_accrual_weight(boundary[-length(boundary)], 20, 0.1)
  expect_equal(
    .meta_grid_pmf(
      "plnorm", args,
      cutoff = 20, pwindow = 1, swindow = 1, growth_rate = 0.1, accrual = 1L
    ),
    expected / sum(expected),
    tolerance = 1e-12
  )
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

test_that(".meta_summary_terms fits a quantile with a reported standard error on the delay scale", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 60, pwindow = 1, swindow = 1, value = 6, report_se = 0.4,
    quantile_p = 0.75, growth_rate = 0, trunc_design = 0L
  )
  # A study reports a quantile's standard error on the delay scale, so the
  # reported value is compared with the implied quantile on that scale, with
  # the standard error as reported.
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["observed"]], 6)
  expect_equal(
    terms[["implied"]], stats::qlnorm(0.75, args$meanlog, args$sdlog),
    tolerance = 1e-10
  )
  expect_identical(terms[["se"]], 0.4)
  expect_equal(
    .meta_row_log_lik(slots, "plnorm", args),
    stats::dnorm(6, stats::qlnorm(0.75, 1.5, 0.5), 0.4, log = TRUE),
    tolerance = 1e-10
  )
})

test_that(".meta_summary_terms reads the implied quantile of a naive study off its grid", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 0L, cens_adjusted = 0L,
    cutoff = 20, pwindow = 1, swindow = 1, value = 5, report_se = 0.6,
    quantile_p = 0.6, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  expect_equal(
    terms[["implied"]], .meta_node_quantile(nodes, 0.6),
    tolerance = 1e-12
  )
  expect_identical(terms[["se"]], 0.6)
})

test_that(".meta_summary_terms guards a reported quantile se away from zero", { # nolint: line_length_linter.
  args <- list(meanlog = 1.5, sdlog = 0.5)
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 250L, trunc_adjusted = 0L, cens_adjusted = 0L,
    cutoff = 20, pwindow = 1, swindow = 1, value = 5, report_se = 1e-9,
    quantile_p = 0.6, growth_rate = 0, trunc_design = 0L
  )
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_identical(terms[["se"]], .meta_min_prob_se())
})

test_that("a quantile far into the tail gives a slope on the delay scale rather than a wall", { # nolint: line_length_linter.
  # A 95th percentile reported as 40 days with a standard error of 2 when the
  # true one is 11.3 is rejected by both scales, but on the probability scale
  # the density at the reported value collapses and the converted standard
  # error hits its floor, giving a log likelihood of order -1e7 whose
  # gradient is dominated by the floor. On the delay scale it is the normal
  # density it claims to be, everywhere.
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = 0L, trunc_adjusted = 1L, cens_adjusted = 1L,
    cutoff = 800, pwindow = 1, swindow = 1, value = 40, report_se = 2,
    quantile_p = 0.95, growth_rate = 0, trunc_design = 0L
  )
  for (meanlog in c(1.6, 2.0, 2.4)) {
    expect_equal(
      .meta_row_log_lik(slots, "plnorm", list(meanlog = meanlog, sdlog = 0.5)),
      stats::dnorm(40, stats::qlnorm(0.95, meanlog, 0.5), 2, log = TRUE),
      tolerance = 1e-8
    )
  }
  expect_gt(
    .meta_row_log_lik(slots, "plnorm", list(meanlog = 1.6, sdlog = 0.5)),
    -110
  )
})

test_that("a delay scale quantile se is calibrated against a bootstrapped median", { # nolint: line_length_linter.
  set.seed(126)
  args <- list(meanlog = 1.5, sdlog = 0.5)
  cutoff <- 20
  study_n <- 200
  pool <- rlnorm(2e5, args$meanlog, args$sdlog)
  pool <- pool[pool <= cutoff]
  medians <- vapply(
    seq_len(2000),
    function(i) {
      return(stats::median(sample(pool, study_n, replace = TRUE)))
    },
    numeric(1)
  )
  slots <- list(
    lower = 0,
    obs_type = 4L, study_n = study_n, trunc_adjusted = 0L, cens_adjusted = 1L,
    cutoff = cutoff, pwindow = 1, swindow = 1, value = stats::median(pool),
    report_se = stats::sd(medians), quantile_p = 0.5, growth_rate = 0,
    trunc_design = 0L
  )
  # The implied median of the truncated estimand at the truth sits within the
  # bootstrap spread of the reported one, so the row's standardised residual
  # is small at the true parameters.
  terms <- .meta_summary_terms(slots, "plnorm", args)
  expect_lt(
    abs(terms[["observed"]] - terms[["implied"]]) / terms[["se"]], 0.5
  )
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

test_that(".meta_quantile_set_ll floors a cell the study saw but the estimand cannot reach", { # nolint: line_length_linter.
  slots <- list(
    lower = 0,
    cutoff = 60, pwindow = 1, swindow = 1, trunc_adjusted = 1L,
    cens_adjusted = 1L, growth_rate = 0, trunc_design = 0L
  )
  # The cell below the reported median holds no mass a double can represent,
  # so its probability is floored rather than sent to zero. That keeps the
  # log likelihood finite, as Stan's log scale differences do, so that a
  # single badly misfitting draw cannot break loo.
  unreachable <- .meta_quantile_set_ll(
    1e-8, 50L, 100, "plnorm", list(meanlog = 5, sdlog = 0.1), slots
  )
  expect_true(is.finite(unreachable))
  expect_equal(
    unreachable, lchoose(100, 50) + 50 * log(.meta_cell_floor()),
    tolerance = 1e-8
  )
  # A cell the study saw nothing in contributes nothing however small it is.
  expect_true(is.finite(.meta_quantile_set_ll(
    1e-8, 0L, 100, "plnorm", list(meanlog = 5, sdlog = 0.1), slots
  )))
})

test_that(".meta_quantile_set_ll reads a single integer day quantile as its crossing cell", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  n <- 1000
  for (design in list(
    list(cens_adjusted = 0L, trunc_design = 0L, growth_rate = 0),
    list(cens_adjusted = 3L, trunc_design = 0L, growth_rate = 0),
    list(cens_adjusted = 0L, trunc_design = 1L, growth_rate = 0.1)
  )) {
    slots <- c(
      list(
        lower = 0, cutoff = 30, pwindow = 1, swindow = 1,
        trunc_adjusted = 0L
      ),
      design
    )
    shift <- .meta_cens_shift(design$cens_adjusted, 1, 1)
    # "The median is 5 days" says the empirical distribution function crossed
    # a half between 4 and 5, so N_{<= 4} < ceiling(n p) <= N_{<= 5}, with the
    # counts binomial on the uncorrected grid distribution function.
    accrual <- .meta_accrual_flag(0L, design$trunc_design)
    mass <- .meta_grid_pmf(
      "plnorm", args, 0, 30, 1, 1, design$growth_rate, accrual
    )
    grid_cdf <- c(0, cumsum(mass))
    log_tail <- function(k, prob) {
      return(stats::pbinom(
        k - 1, n, prob,
        lower.tail = FALSE, log.p = TRUE
      ))
    }
    for (p in c(0.25, 0.5, 0.9)) {
      y <- 5
      k <- ceiling(n * p)
      upper <- log_tail(k, grid_cdf[y + 2])
      lower <- log_tail(k, grid_cdf[y + 1])
      expected <- upper + log(-expm1(lower - upper))
      actual <- .meta_quantile_set_ll(
        y + shift, round(n * p), n, "plnorm", args, slots,
        p = p
      )
      expect_equal(actual, expected, tolerance = 1e-8)
      # The tails are taken on the log scale, so a crossing the estimand
      # puts far into its tail stays finite.
      expect_true(is.finite(actual))
    }
    # The event is on the cumulative counts, so a study of any size can put
    # the crossing in the reported cell with a probability that stays below
    # one and above zero across a wide range of parameters.
    profile <- vapply(
      seq(1.3, 1.9, by = 0.05),
      function(m) {
        return(.meta_quantile_set_ll(
          5 + shift, 500L, n, "plnorm", list(meanlog = m, sdlog = 0.5),
          slots,
          p = 0.5
        ))
      },
      numeric(1)
    )
    expect_true(all(is.finite(profile)))
    expect_lte(max(profile), 0)
    # A reported median far above every delay the estimand allows is a
    # crossing the model finds very unlikely rather than impossible, so a
    # random initial value does not give a log likelihood of -Inf.
    far <- .meta_quantile_set_ll(
      8 + shift, 158L, 315L, "pgamma", list(shape = 7.4, scale = 0.02),
      slots,
      p = 0.5
    )
    expect_true(is.finite(far))
    expect_lt(far, -1e4)
  }
})

test_that(".meta_log_binom_upper matches pbinom above its switch and is finite below", { # nolint: line_length_linter.
  m <- c(3L, 10L, 50L)
  size <- c(20L, 100L, 300L)
  expect_identical(
    .meta_log_binom_upper(m, size, 0.2),
    stats::pbinom(m - 1, size, 0.2, lower.tail = FALSE, log.p = TRUE)
  )
  # Below the switch the tail is summed term by term, which agrees with
  # the distribution function wherever that is representable.
  expect_equal(
    .meta_log_binom_upper(m, size, 2e-12),
    stats::pbinom(m - 1, size, 2e-12, lower.tail = FALSE, log.p = TRUE),
    tolerance = 1e-10
  )
  # Far above the mean the same sum takes over from the distribution
  # function.
  expect_equal(
    .meta_log_binom_upper(c(150L, 190L), 200L, 0.3),
    stats::pbinom(c(149L, 189L), 200L, 0.3, lower.tail = FALSE, log.p = TRUE),
    tolerance = 1e-10
  )
  tiny <- .meta_log_binom_upper(m, size, 1e-300)
  expect_true(all(is.finite(tiny)))
  expect_true(all(tiny < -1000))
  expect_identical(.meta_log_binom_upper(c(0L, 5L), 10L, c(0.3, 1)), c(0, 0))
})

test_that("a single integer day quantile carries information that saturates in n", { # nolint: line_length_linter.
  # The multinomial on the continuity corrected grid claims a curvature that
  # grows like n, while the crossing event stops sharpening once the binomial
  # spread of the crossing is narrower than a day.
  slots <- list(
    lower = 0, cutoff = 30, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 0L, trunc_design = 0L, growth_rate = 0
  )
  curvature <- function(n) {
    ll <- function(m) {
      return(.meta_quantile_set_ll(
        5, round(n / 2), n, "plnorm", list(meanlog = m, sdlog = 0.5), slots,
        p = 0.5
      ))
    }
    return(-(ll(1.61) - 2 * ll(1.6) + ll(1.59)) / 0.01^2)
  }
  expect_lt(curvature(10000) / curvature(100), 10)
})

test_that(".meta_quantile_set_ll merges coincident reported quantiles into one cell", { # nolint: line_length_linter.
  args <- list(meanlog = 1.6, sdlog = 0.5)
  slots <- list(
    lower = 0, cutoff = 30, pwindow = 1, swindow = 1, trunc_adjusted = 0L,
    cens_adjusted = 0L, trunc_design = 0L, growth_rate = 0
  )
  n <- 30
  p <- c(0.25, 0.5, 0.75)
  counts <- .meta_quantile_counts(p, n)
  # A median and an upper quartile both reported as 5 days are two
  # constraints on the empirical distribution function at the same cell,
  # which the multinomial reads as one cell holding both counts.
  merged <- .meta_quantile_set_ll(
    c(4, 5, 5), counts, n, "plnorm", args, slots,
    p = p
  )
  direct <- .meta_quantile_set_ll(
    c(4, 5), counts[c(1, 3)], n, "plnorm", args, slots,
    p = p[c(1, 3)]
  )
  expect_equal(merged, direct, tolerance = 1e-12)
  expect_true(is.finite(merged))
})

test_that("as_epidist_meta_model accepts coincident quantiles from an integer day study", { # nolint: line_length_linter.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = "quantile", value = c(4, 5, 5),
    p = c(0.25, 0.5, 0.75), n = 30, relative_obs_time = 20,
    trunc_adjusted = FALSE, cens_adjusted = 0, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, 6L)
  expect_identical(meta$group_len, 3L)
  expect_no_error(assert_epidist(meta))
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
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = c("A", "A"),
      type = c("sd", "mean"),
      value = c(3.6, 7.5),
      n = c(120, 120),
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(nrow(meta), 1L)
  expect_identical(meta$obs_type, 5L)
  expect_identical(meta$group_len, 2L)
  # The mean is stored first so that the bivariate normal knows which is which.
  expect_identical(.meta_members(meta)$value, c(7.5, 3.6))
})

test_that("as_epidist_meta_model groups the quantiles of one study into a set", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A",
      type = "quantile",
      value = c(9.4, 4.2, 6.1),
      p = c(0.75, 0.25, 0.5),
      n = 60,
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(nrow(meta), 1L)
  expect_identical(meta$obs_type, 6L)
  expect_identical(meta$group_len, 3L)
  expect_identical(.meta_members(meta)$value, c(4.2, 6.1, 9.4))
  expect_identical(.meta_members(meta)$count, c(15L, 30L, 45L))
})

test_that("as_epidist_meta_model keeps a single quantile fittable on its own", {
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A", type = "quantile", value = 6.1, p = 0.5, n = 60,
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, 6L)
  expect_identical(meta$group_len, 1L)
})

test_that("as_epidist_meta_model leaves a summary with a reported standard error ungrouped", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = c("A", "A"),
      type = c("mean", "sd"),
      value = c(7.5, 3.6),
      se = c(0.3, NA),
      n = c(120, 120),
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(2L, 3L))
  expect_true(all(meta$group_len == 0L))
})

test_that("as_epidist_meta_model does not group summaries that differ in a covariate", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = c("A", "A"),
      type = c("mean", "sd"),
      value = c(7.5, 3.6),
      n = c(120, 120),
      setting = c("hospital", "community"),
      stringsAsFactors = FALSE
    )
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
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A",
      type = c("mean", "mean", "sd"),
      value = c(7.5, 8.1, 3.6),
      n = 120,
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_identical(meta$obs_type, c(5L, 2L))
  expect_identical(.meta_members(meta)$value, c(7.5, 3.6))
})

test_that("as_epidist_meta_model errors on quantiles that do not increase with their probability", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A",
      type = "quantile",
      value = c(6.1, 4.2),
      p = c(0.25, 0.5),
      n = 60,
      stringsAsFactors = FALSE
    )
  )))
  expect_error(
    suppressMessages(as_epidist_meta_model(estimates = estimates)),
    "must not decrease"
  )
})

test_that("as_epidist_meta_model errors on two quantiles at the same probability", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A",
      type = "quantile",
      value = c(4.2, 6.1),
      p = c(0.5, 0.5),
      n = 60,
      stringsAsFactors = FALSE
    )
  )))
  expect_error(
    suppressMessages(as_epidist_meta_model(estimates = estimates)),
    "same probability"
  )
})

test_that("assert_epidist.epidist_meta_model checks the grouped summary members", { # nolint: line_length_linter.
  estimates <- suppressWarnings(suppressMessages(as_epidist_estimates_data(
    data.frame(
      study = "A",
      type = c("mean", "sd"),
      value = c(7.5, 3.6),
      n = 120,
      stringsAsFactors = FALSE
    )
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  broken <- meta
  suppressWarnings({
    broken$group_len <- 1L
  })
  expect_meta_assert_error(broken, "exactly two")
  overrun <- meta
  suppressWarnings({
    overrun$group_start <- 2L
  })
  expect_meta_assert_error(overrun, "index within")
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

test_that(".meta_n_quad is the floor of the n_quad slot passed to Stan", {
  # The resolution travels with each row as data rather than being compiled
  # into the Stan code, so the chunk carries no placeholder for it.
  chunk <- .stan_chunk(file.path("meta_model", "functions.stan"))
  expect_false(grepl("n_quad_default", chunk, fixed = TRUE))
  expect_identical(.meta_n_quad(), .meta_n_quad_default())
  expect_true(all(prep_meta_estimates$n_quad >= .meta_n_quad()))
  standata <- suppressMessages(
    epidist(prep_meta_estimates, fn = brms::make_standata)
  )
  expect_identical(as.integer(standata$vint9), prep_meta_estimates$n_quad)
})

test_that(".meta_n_quad is configurable and validated", {
  restore <- options(epidist.meta_n_quad = 20L)
  on.exit(options(restore), add = TRUE)
  expect_identical(.meta_n_quad(), 20L)
  # The floor applies to a study whose spread is resolved by fewer intervals
  # than it asks for.
  wide <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(7, 5), n = 100,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 1,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = wide))
  expect_identical(meta$n_quad, 20L)
  # The option also lifts the cap.
  options(epidist.meta_n_quad = 5000L)
  meta <- suppressMessages(as_epidist_meta_model(estimates = wide))
  expect_identical(meta$n_quad, 5000L)
  options(epidist.meta_n_quad = 21L)
  expect_error(.meta_n_quad(), "even number")
  options(epidist.meta_n_quad = 1L)
  expect_error(.meta_n_quad())
})

test_that(".estimates_n_quad reads the spread from what a study reported", {
  # A reported standard deviation, the range of two or more quantiles, and a
  # quarter of a lone location are the spreads a study is resolved to.
  data <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("sd", "sd", "iqr", "iqr", "median"),
    type = c("mean", "sd", "quantile", "quantile", "quantile"),
    value = c(8, 0.5, 5, 9, 6),
    p = c(NA, NA, 0.25, 0.75, 0.5),
    n = 100, relative_obs_time = 40, trunc_adjusted = FALSE,
    cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  spread <- .estimates_spread(data)
  expect_identical(spread[1:2], c(0.5, 0.5))
  expect_identical(spread[3], 4 / (stats::qnorm(0.75) - stats::qnorm(0.25)))
  expect_identical(spread[5], 1.5)
  n_quad <- .estimates_n_quad(data)
  expect_identical(n_quad, as.integer(pmax(
    4 * ceiling(40 / spread), .meta_n_quad()
  )))
  expect_true(all(n_quad %% 2 == 0))
  expect_true(all(n_quad <= .meta_n_quad_max()))
  # The cap binds for a very narrow study, which is what the coarse
  # quadrature warning now reports.
  narrow <- data
  narrow$value[2] <- 0.01
  expect_identical(.estimates_n_quad(narrow)[1], .meta_n_quad_max())
  expect_identical(.estimates_coarse_quadrature(narrow), "sd")
  expect_identical(.estimates_coarse_quadrature(data), character(0))
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

test_that("the quadrature resolution of a study follows its reported spread", { # nolint: line_length_linter.
  # The truncated moments and the nodes of a continuous estimand are Simpson
  # sums over [delay_min, cutoff], so a fixed number of intervals leaves a
  # narrow delay unresolved on a wide grid. On a grid of twenty times the
  # largest reported value that is a spacing of four standard deviations at
  # a coefficient of variation of 0.05, which pins the kurtosis at its floor
  # and puts the standard deviation out by more than a factor of two. The
  # resolution is therefore chosen per study from its reported spread, and
  # held in the n_quad slot of its rows.
  mean_delay <- 7
  cutoff <- 20 * mean_delay
  trunc_lnorm <- function(args, cutoff) {
    z <- (log(cutoff) - args$meanlog) / args$sdlog
    raw_moment <- vapply(
      1:4,
      function(k) {
        return(
          exp(k * args$meanlog + k^2 * args$sdlog^2 / 2) *
            stats::pnorm(z - k * args$sdlog) / stats::pnorm(z)
        )
      },
      numeric(1)
    )
    return(.meta_central_from_raw(raw_moment))
  }
  row_slots <- function(meta, study) {
    study_row <- meta[meta$study == study, ]
    return(list(
      lower = study_row$delay_min, cutoff = study_row$relative_obs_time,
      pwindow = study_row$pwindow, swindow = study_row$swindow,
      trunc_adjusted = study_row$trunc_adjusted,
      cens_adjusted = study_row$cens_adjusted,
      growth_rate = study_row$growth_rate,
      trunc_design = study_row$trunc_design,
      n_quad = study_row$n_quad
    ))
  }
  for (cv in c(0.05, 0.1, 0.2, 0.5)) {
    var_log <- log1p(cv^2)
    args <- list(
      meanlog = log(mean_delay) - var_log / 2, sdlog = sqrt(var_log)
    )
    estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
      study = c("cohort", "cohort", "growth", "growth"),
      type = c("mean", "sd", "mean", "sd"),
      value = c(mean_delay, cv * mean_delay, mean_delay, cv * mean_delay),
      n = 200,
      relative_obs_time = c(cutoff, cutoff, Inf, Inf),
      trunc_adjusted = c(FALSE, FALSE, TRUE, TRUE),
      cens_adjusted = c(1, 1, 2, 2),
      growth_rate = c(0, 0, 0.1, 0.1),
      stringsAsFactors = FALSE
    )))
    meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
    expect_true(all(meta$n_quad >= .meta_n_quad()))
    expect_true(all(meta$n_quad %% 2 == 0))
    # A truncated continuous study has the closed form truncated lognormal
    # moments as its reference.
    cohort <- .meta_row_moments(row_slots(meta, "cohort"), "plnorm", args)
    exact <- trunc_lnorm(args, cutoff)
    expect_equal(cohort[["mean"]], exact[["mean"]], tolerance = 1e-4)
    expect_equal(cohort[["sd"]], exact[["sd"]], tolerance = 1e-2)
    expect_equal(cohort[["kurtosis"]], exact[["kurtosis"]], tolerance = 1e-2)
    # A growing primary event has no closed form, so a fine evaluation of the
    # same quadrature stands in for it, for the moments and for the chord
    # inverse an implied quantile is read off.
    slots <- row_slots(meta, "growth")
    growth <- .meta_row_moments(slots, "plnorm", args)
    q90 <- .meta_node_quantile(.meta_implied_nodes("plnorm", args, slots), 0.9)
    slots$n_quad <- 20000L
    fine <- .meta_row_moments(slots, "plnorm", args)
    q90_fine <- .meta_node_quantile(
      .meta_implied_nodes("plnorm", args, slots), 0.9
    )
    expect_equal(growth[["mean"]], fine[["mean"]], tolerance = 1e-4)
    expect_equal(growth[["sd"]], fine[["sd"]], tolerance = 1e-2)
    expect_equal(growth[["kurtosis"]], fine[["kurtosis"]], tolerance = 1e-2)
    expect_equal(q90, q90_fine, tolerance = 1e-2)
  }
  # A narrow delay needs more intervals than a wide one, and the floor set by
  # the option still applies.
  narrow <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("narrow", "narrow", "wide", "wide"),
    type = c("mean", "sd", "mean", "sd"),
    value = c(7, 0.35, 7, 3.5), n = 200, relative_obs_time = 140,
    trunc_adjusted = FALSE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  restore <- options(epidist.meta_n_quad = 200L)
  on.exit(options(restore), add = TRUE)
  meta <- suppressMessages(as_epidist_meta_model(estimates = narrow))
  expect_gt(meta$n_quad[meta$study == "narrow"], 1000)
  expect_identical(meta$n_quad[meta$study == "wide"], 200L)
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

test_that(".meta_implied_moments of a truncation adjusted study conditions on delay_min alone", { # nolint: line_length_linter.
  # A study that adjusted for right truncation and only counted delays above
  # delay_min reported the moments of tau | tau > delay_min, which is the
  # estimand its quantile rows already use. Those moments must not depend on
  # max_delay, which for a heavy tailed delay cuts the tail the standard
  # deviation and kurtosis live in.
  left_lnorm <- function(args, lower) {
    raw_moment <- vapply(
      seq_len(4),
      function(k) {
        # On the log scale the integrand is a shifted normal density, which
        # underflows rather than overflows far into the tail.
        return(stats::integrate(
          function(z) {
            return(exp(
              k * z + stats::dnorm(z, args$meanlog, args$sdlog, log = TRUE)
            ))
          },
          log(lower), Inf,
          rel.tol = 1e-10
        )$value)
      },
      numeric(1)
    )
    tail_mass <- stats::plnorm(
      lower, args$meanlog, args$sdlog,
      lower.tail = FALSE
    )
    return(.meta_central_from_raw(raw_moment / tail_mass))
  }
  lnorm_args <- function(mean, sd) {
    var_log <- log1p((sd / mean)^2)
    return(list(meanlog = log(mean) - var_log / 2, sdlog = sqrt(var_log)))
  }
  delay_min <- 3
  for (reported in list(c(24, 40), c(10, 25))) {
    args <- lnorm_args(reported[1], reported[2])
    max_delay <- ceiling(20 * max(reported))
    exact <- left_lnorm(args, delay_min)
    for (cens_adjusted in c(1L, 4L)) {
      pwindow <- if (cens_adjusted == 4L) 2 else 1
      moments <- .meta_implied_moments(
        "plnorm", args,
        lower = delay_min, cutoff = max_delay, pwindow = pwindow, swindow = 1,
        trunc_adjusted = 1L, cens_adjusted = cens_adjusted, growth_rate = 0
      )
      if (cens_adjusted == 4L) {
        # Code 4 is the primary censored delay left truncated at
        # delay_min + pwindow / 2 and moved down by pwindow / 2, so it is
        # checked against its own simulation below rather than the delay.
        next
      }
      expect_equal(moments[["mean"]], exact[["mean"]], tolerance = 1e-4)
      expect_equal(moments[["sd"]], exact[["sd"]], tolerance = 1e-4)
      expect_equal(
        moments[["kurtosis"]], exact[["kurtosis"]],
        tolerance = 1e-3
      )
      expect_equal(
        moments[["skewness"]], exact[["skewness"]],
        tolerance = 1e-3
      )
      # The reported quantiles of the same study describe the same estimand.
      median <- stats::qlnorm(
        1 - 0.5 * stats::plnorm(
          delay_min, args$meanlog, args$sdlog,
          lower.tail = FALSE
        ),
        args$meanlog, args$sdlog
      )
      expect_equal(
        .meta_implied_prob(
          median, "plnorm", args,
          lower = delay_min, cutoff = max_delay, pwindow = 1, swindow = 1,
          trunc_adjusted = 1L, cens_adjusted = 1L, growth_rate = 0
        ),
        0.5,
        tolerance = 1e-8
      )
    }
  }
  # The uniform single interval approximation has the same identity, with
  # the primary censored distribution function in place of the delay's.
  set.seed(205)
  args <- lnorm_args(10, 25)
  pwindow <- 2
  moments <- .meta_implied_moments(
    "plnorm", args,
    lower = delay_min, cutoff = 500, pwindow = pwindow, swindow = 1,
    trunc_adjusted = 1L, cens_adjusted = 2L, growth_rate = 0
  )
  n_sim <- 4e6
  observed <- stats::runif(n_sim, 0, pwindow) +
    stats::rlnorm(n_sim, args$meanlog, args$sdlog)
  observed <- observed[observed > delay_min]
  expect_equal(moments[["mean"]], mean(observed), tolerance = 0.01)
  # A heavy tail makes the simulated standard deviation noisy, so it is only
  # asked to sit nearer the untruncated value than the max_delay truncated
  # one.
  truncated <- .meta_pcens_trunc_moments(
    "plnorm", args, delay_min, 500, pwindow, 0
  )
  expect_gt(moments[["sd"]], truncated[["sd"]] * 1.1)
  expect_equal(moments[["sd"]], stats::sd(observed), tolerance = 0.1)
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
    # exact. A continuous one is inverted through the family quantile
    # function where one exists, and otherwise refined by Newton steps from
    # the bracketing chord, so neither is limited by the node spacing.
    for (p in c(0.1, 0.5, 0.9)) {
      value <- .meta_node_quantile(nodes, p, "plnorm", args, slots)
      round_trip <- .meta_implied_probs(value, "plnorm", args, slots)
      if (cens_adjusted %in% c(0, 3)) {
        expect_equal(round_trip, p, tolerance = 1e-12)
      } else if (cens_adjusted == 1) {
        expect_lt(abs(round_trip - p), 1e-10)
      } else {
        expect_lt(abs(round_trip - p), 1e-8)
      }
      # The chord alone is only as accurate as the node spacing.
      chord <- .meta_node_quantile(nodes, p)
      if (!cens_adjusted %in% c(0, 3)) {
        expect_gt(
          abs(.meta_implied_probs(chord, "plnorm", args, slots) - p), 1e-5
        )
      }
    }
  }
})

test_that(".meta_node_quantile refines a gamma estimand by Newton steps", {
  args <- list(shape = 3, scale = 2.5)
  for (cens_adjusted in c(1, 2)) {
    slots <- list(
      lower = 1, cutoff = 60, pwindow = 1, swindow = 1,
      trunc_adjusted = 0L, cens_adjusted = cens_adjusted, growth_rate = 0,
      trunc_design = 0L
    )
    nodes <- .meta_implied_nodes("pgamma", args, slots)
    for (p in c(0.1, 0.5, 0.9)) {
      value <- .meta_node_quantile(nodes, p, "pgamma", args, slots)
      round_trip <- .meta_implied_probs(value, "pgamma", args, slots)
      expect_lt(abs(round_trip - p), 1e-8)
    }
  }
})

test_that(".meta_node_quantile leaves an accrual estimand on its chord", {
  args <- list(meanlog = 1.6, sdlog = 0.6)
  slots <- list(
    lower = 0, cutoff = 40, pwindow = 1, swindow = 1,
    trunc_adjusted = 0L, cens_adjusted = 1L, growth_rate = 0.1,
    trunc_design = 1L
  )
  nodes <- .meta_implied_nodes("plnorm", args, slots)
  # The accrual estimand is defined by linear interpolation between its
  # nodes, so the chord is already its exact inverse.
  expect_identical(
    .meta_node_quantile(nodes, 0.5, "plnorm", args, slots),
    .meta_node_quantile(nodes, 0.5)
  )
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
    new_epidist_multivariate(
      value = c(mean = 6.2, sd = 3.1),
      vcov = covariance,
      params = c("mean", "sd")
    ),
    study = "A",
    relative_obs_time = 30,
    trunc_adjusted = FALSE,
    cens_adjusted = 0
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
  suppressWarnings({
    meta$delay_min <- meta$relative_obs_time
  })
  expect_meta_assert_error(meta, "must be below")
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
  suppressWarnings({
    wide$delay_upr[1] <- wide$delay_upr[1] + 1
  })
  expect_meta_assert_error(wide, "must equal")
  early <- prep_meta_individual
  suppressWarnings({
    early$relative_obs_time[1] <- early$delay_upr[1] - 0.5
  })
  expect_meta_assert_error(early, "`relative_obs_time` must be greater")
  truncated <- prep_meta_individual
  suppressWarnings({
    truncated$delay_min[1] <- truncated$delay_lwr[1] + 1
  })
  expect_meta_assert_error(truncated, "`delay_lwr` must be greater")
})

test_that("assert_epidist.epidist_meta_model checks the summary rows", {
  small <- prep_meta_estimates
  suppressWarnings({
    small$study_n <- 1L
  })
  expect_meta_assert_error(small, "at least 2")
  unbounded <- prep_meta_estimates
  suppressWarnings({
    unbounded$relative_obs_time[1] <- Inf
  })
  expect_meta_assert_error(unbounded, "finite grid cutoff")
  narrow <- prep_meta_estimates
  suppressWarnings({
    narrow$swindow[1] <- narrow$relative_obs_time[1] + 1
  })
  expect_meta_assert_error(narrow, "grid cutoff for summary rows")
  quantile_row <- which(prep_meta_estimates$obs_type == 6L)[1]
  flat <- prep_meta_estimates
  suppressWarnings({
    flat$quantile_p[quantile_row] <- 0
  })
  expect_meta_assert_error(flat, "strictly between 0 and 1")
})

test_that("assert_epidist.epidist_meta_model checks a covariance matrix row", {
  covariance <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  estimates <- suppressMessages(as_epidist_estimates_data(
    new_epidist_multivariate(
      value = c(mean = 6.2, sd = 3.1),
      vcov = covariance,
      params = c("mean", "sd")
    ),
    study = "A",
    relative_obs_time = 30,
    trunc_adjusted = FALSE,
    cens_adjusted = 0
  ))
  meta <- suppressMessages(as_epidist_meta_model(estimates))
  empty <- meta
  suppressWarnings({
    empty$group_len <- 0L
  })
  expect_meta_assert_error(empty, "at least one grouped summary member")
  # The factor of a two by two matrix has four entries, so a row starting at
  # the second of them runs off the end of the flat vector passed to Stan.
  overrun <- meta
  suppressWarnings({
    overrun$chol_start <- 2L
  })
  expect_meta_assert_error(overrun, "full Cholesky factor")
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
  suppressWarnings({
    none$group_len <- 0L
  })
  expect_meta_assert_error(none, "at least one grouped summary member")
  falling <- members
  falling$value <- c(8, 4)
  expect_error(
    assert_epidist(.meta_set_members(meta, falling)), "must not decrease"
  )
  coincident <- members
  coincident$value <- c(4, 4)
  expect_no_error(assert_epidist(.meta_set_members(meta, coincident)))
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

test_that("as_epidist_meta_model defaults to a uniform primary event", {
  expect_identical(attr(prep_meta_obs, "primary"), "uniform")
  family <- epidist_family(prep_meta_obs)
  expect_identical(family$primary, "uniform")
  expect_false("pgrowth" %in% family$dpars)
  code <- suppressMessages(epidist(prep_meta_obs, fn = brms::make_stancode))
  expect_no_match(code, "b_pgrowth", fixed = TRUE)
  expect_match(code, "1, primary_params", fixed = TRUE)
})

test_that("an expgrowth primary event adds a pgrowth parameter to the meta model", { # nolint: line_length_linter.
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    estimates = sim_estimates, primary = "expgrowth"
  ))
  expect_identical(attr(model, "primary"), "expgrowth")
  family <- epidist_family(model)
  expect_identical(family$primary, "expgrowth")
  expect_true("pgrowth" %in% family$dpars)
  code <- suppressMessages(epidist(
    model,
    formula = brms::bf(mu ~ 1, pgrowth ~ 1), fn = brms::make_stancode
  ))
  expect_match(code, "b_pgrowth", fixed = TRUE)
  # The registry id selects expgrowth within primarycensored for the
  # individual level rows, with pgrowth in scope through dpars_A.
  expect_match(code, "2, {pgrowth}", fixed = TRUE)
  expect_match(code, "real mu, real sigma, real pgrowth", fixed = TRUE)
  expect_match(code, "real primary_lpdf", fixed = TRUE)
})

test_that("the meta model accepts an expgrowth primary event for aggregate data", { # nolint: line_length_linter.
  model <- suppressMessages(as_epidist_meta_model(
    agg_sim_obs,
    primary = "expgrowth"
  ))
  expect_identical(attr(model, "primary"), "expgrowth")
  expect_true("pgrowth" %in% epidist_family(model)$dpars)
})

test_that("the meta model primary event survives the data transform", {
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    estimates = sim_estimates, primary = "expgrowth"
  ))
  family <- epidist_family(model)
  formula <- epidist_formula(model, family, brms::bf(mu ~ 1, pgrowth ~ 1))
  transformed <- suppressMessages(
    epidist_transform_data_model(model, family, formula)
  )
  expect_identical(attr(transformed, "primary"), "expgrowth")
  expect_s3_class(transformed, "epidist_meta_model")
})

test_that("the meta model rejects an unsupported primary event", {
  expect_error(as_epidist_meta_model(sim_obs, primary = "gaussian"))
  expect_error(new_epidist_meta_model(prep_meta_obs, primary = "gaussian"))
})

test_that("summaries only meta models are uniform and refuse a primary event", { # nolint: line_length_linter.
  expect_identical(attr(prep_meta_estimates, "primary"), "uniform")
  expect_false("pgrowth" %in% epidist_family(prep_meta_estimates)$dpars)
  # Summary rows tilt the primary event with their growth_rate metadata, so
  # a primary event distribution has nothing to act on.
  expect_error(
    as_epidist_meta_model(estimates = sim_estimates, primary = "expgrowth"),
    "growth_rate"
  )
  expect_error(
    as_epidist_meta_model(sim_estimates, primary = "expgrowth"),
    "growth_rate"
  )
})

test_that("the meta model log likelihood uses the fitted primary event for individual rows", { # nolint: line_length_linter.
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    primary = "expgrowth"
  ))
  family <- epidist_family(model)
  prep <- structure(
    list(
      data = list(
        Y = 5, vint1 = 1L, vreal1 = 12, vreal2 = 1, vreal3 = 1, vreal4 = 6,
        vreal5 = 2
      ),
      dpars = list(
        mu = matrix(1.5, nrow = 3, ncol = 1),
        sigma = matrix(0.5, nrow = 3, ncol = 1),
        pgrowth = matrix(c(0.2, 0.4, 0.6), nrow = 3, ncol = 1)
      ),
      ndraws = 3,
      nobs = 1,
      family = list(primary = "expgrowth")
    ),
    class = "brmsprep"
  )
  log_lik <- family$log_lik(i = 1, prep)
  expected <- vapply(
    seq_len(prep$ndraws),
    function(draw) {
      return(primarycensored::dpcens(
        x = 5,
        pdist = stats::plnorm,
        pwindow = 1,
        swindow = 1,
        L = 2,
        D = 12,
        dprimary = primarycensored::dexpgrowth,
        dprimary_args = list(r = prep$dpars$pgrowth[draw, 1]),
        log = TRUE,
        meanlog = 1.5,
        sdlog = 0.5
      ))
    },
    numeric(1)
  )
  expect_equal(log_lik, expected, tolerance = 1e-8)
  # The family built from the model carries the primary event, so a prep
  # without one still uses it.
  prep$family <- list()
  expect_equal(family$log_lik(i = 1, prep), expected, tolerance = 1e-8)
  # A uniform primary event gives a different answer, so the check above
  # cannot pass with the primary event ignored.
  uniform <- epidist_family(prep_meta_individual)
  expect_false(isTRUE(all.equal(uniform$log_lik(i = 1, prep), expected)))
})

test_that("the meta model posterior predictions use the fitted primary event", { # nolint: line_length_linter.
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    primary = "expgrowth"
  ))
  family <- epidist_family(model)
  prep <- structure(
    list(
      data = list(
        Y = 5, vint1 = 1L, vreal1 = 30, vreal2 = 1, vreal3 = 1, vreal4 = 6,
        vreal5 = 0
      ),
      dpars = list(
        mu = matrix(1.5, nrow = 50, ncol = 1),
        sigma = matrix(0.5, nrow = 50, ncol = 1),
        pgrowth = matrix(5, nrow = 50, ncol = 1)
      ),
      ndraws = 50,
      nobs = 1,
      family = list(primary = "expgrowth")
    ),
    class = "brmsprep"
  )
  set.seed(101)
  growing <- family$posterior_predict(i = 1, prep)
  # Without a primary event on the prep the family's own is used, so a
  # uniform family draws a uniform primary event.
  prep$family <- list()
  set.seed(101)
  uniform <- epidist_family(prep_meta_individual)$posterior_predict(
    i = 1, prep
  )
  expect_false(isTRUE(all.equal(mean(growing), mean(uniform))))
})
