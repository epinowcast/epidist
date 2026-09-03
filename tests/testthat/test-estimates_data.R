# fmt: skip file
est_df <- data.frame(
  study = c("A", "A", "B", "C"),
  type = c("mean", "sd", "quantile", "mean"),
  value = c(7.2, 3.8, 9.0, 6.4),
  p = c(NA, NA, 0.9, NA),
  n = c(120, 120, 80, 200),
  relative_obs_time = c(20, 20, Inf, 30),
  trunc_adjusted = c(FALSE, FALSE, TRUE, FALSE),
  cens_adjusted = c(0, 0, 1, 2),
  stringsAsFactors = FALSE
)

test_that("as_epidist_estimates_data returns an object with the correct classes", { # nolint: line_length_linter.
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_s3_class(est, "data.frame")
  expect_s3_class(est, "epidist_estimates_data")
})

test_that("as_epidist_estimates_data adds all required columns with defaults", { # nolint: line_length_linter.
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_named(est, .estimates_required_cols())
  expect_identical(est$pwindow, rep(1, 4))
  expect_identical(est$swindow, rep(1, 4))
  expect_identical(est$growth_rate, rep(0, 4))
  expect_true(all(is.finite(est$max_delay)))
  expect_true(all(est$max_delay > est$swindow))
})

test_that("as_epidist_estimates_data renames user supplied columns", {
  renamed <- est_df
  names(renamed)[names(renamed) == "value"] <- "central"
  names(renamed)[names(renamed) == "n"] <- "sample_size"
  est <- suppressMessages(
    as_epidist_estimates_data(renamed, value = "central", n = "sample_size")
  )
  expect_identical(est$value, est_df$value)
  expect_identical(est$n, est_df$n)
})

test_that("as_epidist_estimates_data messages about assumed study metadata", {
  minimal <- data.frame(
    study = "A", type = "mean", value = 7.2, n = 100,
    stringsAsFactors = FALSE
  )
  msgs <- capture_messages(
    suppressWarnings(as_epidist_estimates_data(minimal))
  )
  expect_true(any(grepl("censoring window", msgs, fixed = TRUE)))
  expect_true(any(grepl("right truncation", msgs, fixed = TRUE)))
  expect_true(any(grepl("max_delay", msgs, fixed = TRUE)))
  expect_true(any(grepl("cens_adjusted", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data assumes no truncation when no observation time is supplied", { # nolint: line_length_linter.
  minimal <- data.frame(
    study = "A", type = "mean", value = 7.2, n = 100,
    stringsAsFactors = FALSE
  )
  est <- suppressWarnings(suppressMessages(as_epidist_estimates_data(minimal)))
  expect_true(est$trunc_adjusted)
  expect_identical(est$relative_obs_time, Inf)
})

test_that("as_epidist_estimates_data warns when it assumes a study adjusted for truncation", { # nolint: line_length_linter.
  minimal <- data.frame(
    study = "A", type = "mean", value = 7.2, n = 100,
    stringsAsFactors = FALSE
  )
  expect_warning(
    suppressMessages(as_epidist_estimates_data(minimal)),
    "adjusted for right truncation"
  )
  expect_warning(
    suppressMessages(as_epidist_estimates_data(minimal)),
    "Checks"
  )
  # A finite observation time means the study is assumed not to have adjusted,
  # which is announced as a message rather than a warning.
  finite <- minimal
  finite$relative_obs_time <- 20
  msgs <- capture_messages(
    expect_no_warning(as_epidist_estimates_data(finite))
  )
  expect_true(any(grepl("did not adjust", msgs, fixed = TRUE)))
  # An explicit column is neither warned about nor messaged.
  explicit <- minimal
  explicit$trunc_adjusted <- TRUE
  msgs <- capture_messages(
    expect_no_warning(as_epidist_estimates_data(explicit))
  )
  expect_false(any(grepl("trunc_adjusted", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data rejects a mean beyond the observation time of an unadjusted study", { # nolint: line_length_linter.
  bad <- est_df
  bad$value[1] <- 25
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "at or beyond its observation time"
  )
  adjusted <- bad
  adjusted$trunc_adjusted[1:2] <- TRUE
  adjusted$relative_obs_time[1:2] <- Inf
  expect_no_error(suppressMessages(as_epidist_estimates_data(adjusted)))
})

test_that("as_epidist_estimates_data rejects a standard error of zero", {
  bad <- est_df
  bad$se <- c(0.4, 0, NA, NA)
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "standard error.*greater than zero"
  )
})

test_that("as_epidist_estimates_data rejects a standard deviation of zero", {
  bad <- est_df
  bad$value[2] <- 0
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "standard deviation.*greater than zero"
  )
})

test_that("as_epidist_estimates_data requires a whole number censoring adjustment code", { # nolint: line_length_linter.
  bad <- est_df
  bad$cens_adjusted[1] <- 1.5
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "Assertion on 'cens_adjusted' failed"
  )
  bad$cens_adjusted[1] <- NA
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "Assertion on 'cens_adjusted' failed"
  )
})

test_that("as_epidist_estimates_data errors on an unsupported summary type", {
  bad <- est_df
  bad$type[1] <- "median"
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "Assertion on 'type' failed"
  )
})

test_that("as_epidist_estimates_data errors when quantile rows have no probability", { # nolint: line_length_linter.
  bad <- est_df
  bad$p[3] <- NA
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "must have a probability"
  )
})

test_that("as_epidist_estimates_data errors when both sample size and standard error are missing", { # nolint: line_length_linter.
  bad <- est_df
  bad$n <- NA_real_
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "sample size"
  )
})

test_that("as_epidist_estimates_data accepts a standard error in place of a sample size", { # nolint: line_length_linter.
  ok <- est_df
  ok$n <- NA_real_
  ok$se <- c(0.4, 0.3, 0.05, 0.6)
  est <- suppressMessages(as_epidist_estimates_data(ok))
  expect_identical(est$se, ok$se)
})

test_that("as_epidist_estimates_data errors when an unadjusted study has no observation time", { # nolint: line_length_linter.
  bad <- est_df
  bad$relative_obs_time <- Inf
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "must have a finite"
  )
})

test_that("as_epidist_estimates_data errors on an unsupported adjustment code", { # nolint: line_length_linter.
  bad <- est_df
  bad$cens_adjusted[1] <- 5
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "Assertion on 'cens_adjusted' failed"
  )
})

test_that("as_epidist_estimates_data accepts both midpoint adjustments", {
  for (code in 3:4) {
    mid <- est_df
    mid$cens_adjusted[1] <- code
    est <- suppressMessages(as_epidist_estimates_data(mid))
    expect_identical(est$cens_adjusted[1], as.integer(code))
  }
})

test_that("as_epidist_estimates_data defaults the truncation design to cohort", { # nolint: line_length_linter.
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_identical(est$trunc_design, rep("cohort", 4))
})

test_that("as_epidist_estimates_data messages about the assumed truncation design", { # nolint: line_length_linter.
  msgs <- capture_messages(as_epidist_estimates_data(est_df))
  expect_true(any(grepl("trunc_design", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data stays quiet about the truncation design when every study adjusted", { # nolint: line_length_linter.
  adjusted <- est_df
  adjusted$trunc_adjusted <- TRUE
  adjusted$relative_obs_time <- Inf
  msgs <- capture_messages(as_epidist_estimates_data(adjusted))
  expect_false(any(grepl("trunc_design", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data accepts an accrual truncation design", {
  accrual <- est_df
  accrual$trunc_design <- c("accrual", "accrual", "cohort", "accrual")
  est <- suppressMessages(as_epidist_estimates_data(accrual))
  expect_identical(est$trunc_design, accrual$trunc_design)
})

test_that("as_epidist_estimates_data renames a supplied truncation design column", { # nolint: line_length_linter.
  renamed <- est_df
  renamed$design <- "accrual"
  est <- suppressMessages(
    as_epidist_estimates_data(renamed, trunc_design = "design")
  )
  expect_identical(est$trunc_design, rep("accrual", 4))
})

test_that("as_epidist_estimates_data errors on an unsupported truncation design", { # nolint: line_length_linter.
  bad <- est_df
  bad$trunc_design <- "calendar"
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "Assertion on 'trunc_design' failed"
  )
})

test_that("as_epidist_estimates_data respects a user supplied max_delay", {
  supplied <- est_df
  supplied$max_delay <- 45
  est <- suppressMessages(as_epidist_estimates_data(supplied))
  expect_identical(est$max_delay, rep(45, 4))
})

test_that("as_epidist_estimates_data warns about a short grid cutoff", {
  short <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(8.3, 7.9), n = 500,
    relative_obs_time = Inf, trunc_adjusted = TRUE, cens_adjusted = 0,
    max_delay = 45, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(short))
  expect_true(any(grepl("short relative", msgs, fixed = TRUE)))
  # the default cutoff is generous enough not to trip the check
  short$max_delay <- NULL
  msgs <- capture_messages(as_epidist_estimates_data(short))
  expect_false(any(grepl("short relative", msgs, fixed = TRUE)))
  # A heavy tailed study is flagged at the default cutoff, where a lognormal
  # matched to its mean and standard deviation still has a tenth of its
  # second moment beyond the cutoff and the grid standard deviation is
  # several percent low.
  heavy <- short
  heavy$value <- c(24, 40)
  msgs <- capture_messages(as_epidist_estimates_data(heavy))
  short_msg <- msgs[grepl("short relative", msgs, fixed = TRUE)]
  expect_length(short_msg, 1)
  expect_true(grepl("max_delay", short_msg, fixed = TRUE))
  expect_true(grepl("\"A\"", short_msg, fixed = TRUE))
  expect_true(grepl("Checks", short_msg, fixed = TRUE))
  expect_identical(
    .estimates_short_cutoff(suppressMessages(as_epidist_estimates_data(heavy))),
    "A"
  )
  # A study reporting a median and an upper quantile is matched through them.
  quantiles <- data.frame(
    study = "B", type = "quantile", value = c(10, 60), p = c(0.5, 0.9),
    n = 50, relative_obs_time = Inf, trunc_adjusted = TRUE,
    cens_adjusted = 0, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_true(any(grepl("short relative", msgs, fixed = TRUE)))
  # Without a median there is nothing to match, so the study is skipped.
  msgs <- capture_messages(as_epidist_estimates_data(quantiles[2, ]))
  expect_false(any(grepl("short relative", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data warns when the quadrature is coarse relative to the delay", { # nolint: line_length_linter.
  # The quadrature spans delay_min to the grid cutoff on a number of
  # intervals chosen from the spread the study reported, so a heavy tailed
  # study whose default max_delay is far above its mean is resolved without
  # a warning.
  heavy <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(24, 40), n = 500,
    relative_obs_time = Inf, trunc_adjusted = TRUE, cens_adjusted = 2,
    growth_rate = 0.1, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(heavy))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
  # A very narrow study needs more intervals than the cap allows.
  narrow <- heavy
  narrow$value <- c(24, 0.1)
  msgs <- capture_messages(as_epidist_estimates_data(narrow))
  expect_true(any(grepl("quadrature", msgs, fixed = TRUE)))
  expect_true(any(grepl("epidist.meta_n_quad", msgs, fixed = TRUE)))
  expect_true(any(grepl("max_delay", msgs, fixed = TRUE)))
  quad_msg <- msgs[grepl("quadrature", msgs, fixed = TRUE)]
  expect_length(quad_msg, 1)
  expect_true(grepl("Checks", quad_msg, fixed = TRUE))
  # Raising the floor above the cap clears it.
  old <- options(epidist.meta_n_quad = 40000)
  on.exit(options(old), add = TRUE)
  msgs <- capture_messages(as_epidist_estimates_data(narrow))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
  options(old)
  # So does a shorter grid.
  narrow$max_delay <- 30
  msgs <- capture_messages(as_epidist_estimates_data(narrow))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
  # A study that adjusted for right truncation with a uniform primary event
  # uses the analytic moments, so its cutoff does not matter.
  narrow$max_delay <- NULL
  narrow$growth_rate <- 0
  msgs <- capture_messages(as_epidist_estimates_data(narrow))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
  # A truncated continuous study reporting only a location is measured
  # against a quarter of it, so it is only flagged when its observation time
  # is very long relative to the delay.
  truncated <- data.frame(
    study = "B", type = "mean", value = 5, n = 200,
    relative_obs_time = 600, trunc_adjusted = FALSE, cens_adjusted = 1,
    stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(truncated))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
  truncated$relative_obs_time <- 5000
  msgs <- capture_messages(as_epidist_estimates_data(truncated))
  expect_true(any(grepl("quadrature", msgs, fixed = TRUE)))
  # A study on the discrete grid is never flagged, because it uses no
  # quadrature.
  truncated$cens_adjusted <- 0
  msgs <- capture_messages(as_epidist_estimates_data(truncated))
  expect_false(any(grepl("quadrature", msgs, fixed = TRUE)))
})

test_that(".estimates_coarse_quadrature names the studies with coarse nodes", {
  data <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c("A", "A", "B", "B", "C"),
    type = c("mean", "sd", "mean", "sd", "mean"),
    value = c(24, 0.05, 24, 0.05, 5),
    n = 500,
    relative_obs_time = c(Inf, Inf, Inf, Inf, 30),
    trunc_adjusted = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    cens_adjusted = c(2, 2, 1, 1, 1),
    growth_rate = c(0.1, 0.1, 0.1, 0.1, 0),
    stringsAsFactors = FALSE
  )))
  expect_identical(.estimates_coarse_quadrature(data), "A")
})

test_that("as_epidist_estimates_data warns about several integer day quantiles from a large study", { # nolint: line_length_linter.
  quantiles <- data.frame(
    study = "A", type = "quantile", value = c(3, 5, 8),
    p = c(0.25, 0.5, 0.75), n = 400, relative_obs_time = 30,
    trunc_adjusted = FALSE, cens_adjusted = 0, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_true(any(grepl("overconfident", msgs, fixed = TRUE)))
  over_msg <- msgs[grepl("overconfident", msgs, fixed = TRUE)]
  expect_length(over_msg, 1)
  expect_true(grepl("Checks", over_msg, fixed = TRUE))
  # A small study, a single quantile or a continuous study does not trip it.
  quantiles$n <- 60
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_false(any(grepl("overconfident", msgs, fixed = TRUE)))
  quantiles$n <- 400
  msgs <- capture_messages(as_epidist_estimates_data(quantiles[2, ]))
  expect_false(any(grepl("overconfident", msgs, fixed = TRUE)))
  quantiles$cens_adjusted <- 1
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_false(any(grepl("overconfident", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data warns about a heavy tailed standard deviation from a small study", { # nolint: line_length_linter.
  # A lognormal with sdlog 1 has kurtosis 114, so the relative standard error
  # of a reported sd, sqrt((kappa - 1) / (4 n)), is 0.53 at n = 100 and the
  # normal sampling likelihood of the sd cannot be trusted.
  heavy <- data.frame(
    study = "A", type = c("mean", "sd"),
    value = c(exp(1.6 + 0.5), exp(1.6 + 0.5) * sqrt(expm1(1))), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(heavy))
  expect_true(any(grepl("relative standard error", msgs, fixed = TRUE)))
  # The message names the row of the input and points at the documentation.
  heavy_msg <- msgs[grepl("relative standard error", msgs, fixed = TRUE)]
  expect_length(heavy_msg, 1)
  expect_true(grepl("\"A\" (row 2)", heavy_msg, fixed = TRUE))
  expect_true(grepl("Checks", heavy_msg, fixed = TRUE))
  # At n = 1000 the relative standard error is 0.17 and the warning is
  # silent, as it is for a lighter tail at n = 100.
  heavy$n <- 1000
  msgs <- capture_messages(as_epidist_estimates_data(heavy))
  expect_false(any(grepl("relative standard error", msgs, fixed = TRUE)))
  light <- heavy
  light$n <- 100
  light$value <- c(exp(1.6 + 0.125), exp(1.6 + 0.125) * sqrt(expm1(0.25)))
  msgs <- capture_messages(as_epidist_estimates_data(light))
  expect_false(any(grepl("relative standard error", msgs, fixed = TRUE)))
  # A standard deviation reported with its own standard error, or without a
  # mean to judge the tail by, is left alone.
  own_se <- heavy
  own_se$n <- 100
  own_se$se <- c(NA, 2)
  msgs <- capture_messages(as_epidist_estimates_data(own_se))
  expect_false(any(grepl("relative standard error", msgs, fixed = TRUE)))
  msgs <- capture_messages(as_epidist_estimates_data(heavy[2, ]))
  expect_false(any(grepl("relative standard error", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data does not warn about a truncated study", {
  truncated <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(8.3, 7.9), n = 500,
    relative_obs_time = 15, trunc_adjusted = FALSE, cens_adjusted = 0,
    stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(truncated))
  expect_false(any(grepl("short relative", msgs, fixed = TRUE)))
})

test_that("as_epidist_estimates_data keys the coarse quantile warning on the smallest quantile", { # nolint: line_length_linter.
  # A study whose smallest quantile sits within ten censoring windows of the
  # smallest delay it counted is flagged even when its largest does not.
  quantiles <- data.frame(
    study = "A", type = "quantile", value = c(5, 30), p = c(0.25, 0.9),
    n = 50, relative_obs_time = Inf, trunc_adjusted = TRUE,
    cens_adjusted = 0, stringsAsFactors = FALSE
  )
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_true(any(grepl("smallest quantile", msgs, fixed = TRUE)))
  expect_true(any(grepl("mean and standard deviation", msgs, fixed = TRUE)))
  coarse_msg <- msgs[grepl("smallest quantile", msgs, fixed = TRUE)]
  expect_length(coarse_msg, 1)
  expect_true(grepl("\"A\" (row 1)", coarse_msg, fixed = TRUE))
  expect_true(grepl("Checks", coarse_msg, fixed = TRUE))
  expect_identical(
    .estimates_coarse_quantiles(
      suppressMessages(as_epidist_estimates_data(quantiles))
    ),
    "A"
  )
  # Quantiles that all sit at least ten windows up the grid are not flagged.
  quantiles$value <- c(12, 30)
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_false(any(grepl("smallest quantile", msgs, fixed = TRUE)))
  # A study that adjusted for censoring is not on the discrete grid.
  quantiles$value <- c(5, 30)
  quantiles$cens_adjusted <- 1
  msgs <- capture_messages(as_epidist_estimates_data(quantiles))
  expect_false(any(grepl("smallest quantile", msgs, fixed = TRUE)))
})

test_that("the default grid cutoff keeps the implied summaries close to the untruncated ones", { # nolint: line_length_linter.
  heavy <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(8.3, 7.9), n = 500,
    relative_obs_time = Inf, trunc_adjusted = TRUE, cens_adjusted = 0,
    stringsAsFactors = FALSE
  )
  est <- suppressMessages(as_epidist_estimates_data(heavy))
  args <- list(meanlog = 1.8, sdlog = 0.8)
  at_default <- .meta_implied_moments(
    "plnorm", args, 0, .estimates_grid_cutoff(est)[1], 1, 1, 1L, 0L, 0
  )
  at_wide <- .meta_implied_moments(
    "plnorm", args, 0, 2000, 1, 1, 1L, 0L, 0
  )
  expect_equal(at_default[["mean"]], at_wide[["mean"]], tolerance = 0.005)
  expect_equal(at_default[["sd"]], at_wide[["sd"]], tolerance = 0.02)
})

test_that("is_epidist_estimates_data returns TRUE for correct input", {
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_true(is_epidist_estimates_data(est))
})

test_that("is_epidist_estimates_data returns FALSE for incorrect input", {
  expect_false(is_epidist_estimates_data(list()))
  expect_false(is_epidist_estimates_data(est_df))
})

test_that("assert_epidist.epidist_estimates_data does not error for correct input", { # nolint: line_length_linter.
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_no_error(assert_epidist(est))
})

test_that("assert_epidist.epidist_estimates_data errors for incorrect input", {
  est <- suppressMessages(as_epidist_estimates_data(est_df))
  expect_error(assert_epidist(est[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_estimates_data"
    assert_epidist(x)
  })
})

test_that("as_epidist_estimates_data errors on a quantile beyond the grid", {
  beyond <- data.frame(
    study = "A", type = "quantile", value = 21, p = 0.9, n = 100,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 1,
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(beyond)),
    "carries no information"
  )
})

test_that("as_epidist_estimates_data errors on a quantile in the top grid cell", { # nolint: line_length_linter.
  # A naive study's grid is continuity corrected, so it reaches one half a
  # cell below the top of the grid.
  top <- data.frame(
    study = "A", type = "quantile", value = 19.6, p = 0.9, n = 100,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0,
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(top)), "carries no information"
  )
  top$value <- 19
  expect_no_error(suppressMessages(as_epidist_estimates_data(top)))
})

test_that(".estimates_quantile_limit allows for the midpoint imputation shift", { # nolint: line_length_linter.
  data <- data.frame(
    trunc_adjusted = rep(FALSE, 4), relative_obs_time = rep(20, 4),
    max_delay = rep(100, 4), swindow = rep(2, 4), pwindow = rep(3, 4),
    cens_adjusted = c(0L, 1L, 3L, 4L)
  )
  # Midpoint imputation of the primary event moves the estimand down by half a
  # primary window, so its largest reportable quantile moves with it.
  expect_identical(.estimates_quantile_limit(data), c(19, 20, 20, 18.5))
})

test_that("as_epidist_estimates_data defaults delay_min to zero and validates it", { # nolint: line_length_linter.
  base <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(7.5, 3.6), n = 120,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0,
    stringsAsFactors = FALSE
  )
  expect_true(all(
    suppressMessages(as_epidist_estimates_data(base))$delay_min == 0
  ))
  beyond <- base
  beyond$delay_min <- 20
  expect_error(
    suppressMessages(as_epidist_estimates_data(beyond)),
    "must be below the grid cutoff"
  )
  above <- base
  above$delay_min <- 8
  expect_error(
    suppressMessages(as_epidist_estimates_data(above)),
    "cannot report a summary below"
  )
})

test_that("as_epidist_estimates_data rejects a code 4 delay_min the primary window moves past the cutoff", { # nolint: line_length_linter.
  # Code 4 places the primary event at the midpoint of its window, so a
  # study that dropped delays below delay_min left truncated its base
  # estimand at delay_min + pwindow / 2. That point has to sit below the
  # grid cutoff as delay_min itself does.
  base <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(9, 2), n = 120,
    relative_obs_time = 12, trunc_adjusted = FALSE, cens_adjusted = 4,
    pwindow = 9, swindow = 1, delay_min = 8, stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(base)),
    "half a"
  )
  fine <- base
  fine$pwindow <- 3
  expect_s3_class(
    suppressMessages(as_epidist_estimates_data(fine)),
    "epidist_estimates_data"
  )
  # The other codes are only held below the cutoff itself.
  fine$cens_adjusted <- 2
  fine$pwindow <- 9
  expect_s3_class(
    suppressMessages(as_epidist_estimates_data(fine)),
    "epidist_estimates_data"
  )
})

test_that("a reported covariance matrix must cover its rows", {
  base <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(7.5, 3.6), n = 120,
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0,
    mvn_id = "A", stringsAsFactors = FALSE
  )))
  good <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  expect_no_error(
    assert_epidist(.estimates_set_vcov(base, list(A = good)))
  )
  expect_error(
    assert_epidist(.estimates_set_vcov(base, list(B = good))),
    "not among the .mvn_id. values"
  )
  expect_error(
    assert_epidist(
      .estimates_set_vcov(base, list(A = matrix(1, nrow = 1)))
    ),
    "covers 2 summaries"
  )
  expect_error(
    assert_epidist(.estimates_set_vcov(
      base, list(A = matrix(c(0.4, 0.1, 0.2, 0.25), nrow = 2))
    )),
    "must be symmetric"
  )
  expect_error(
    assert_epidist(.estimates_set_vcov(
      base, list(A = matrix(c(0.4, 0.5, 0.5, 0.25), nrow = 2))
    )),
    "positive definite"
  )
  expect_error(
    assert_epidist(.estimates_set_vcov(
      base, list(A = matrix(0.1, nrow = 2, ncol = 3))
    )),
    "square and numeric"
  )
  expect_error(
    assert_epidist(.estimates_set_vcov(
      base, list(A = matrix("a", nrow = 2, ncol = 2))
    )),
    "square and numeric"
  )
  expect_error(
    .estimates_set_vcov(base, list(good)),
    "named by"
  )
  with_se <- base
  with_se$se <- 0.2
  expect_error(
    assert_epidist(.estimates_set_vcov(with_se, list(A = good))),
    "must not also report"
  )
})

test_that("a reported covariance matrix needs one set of study metadata", {
  varying <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(7.5, 3.6), n = 120,
    relative_obs_time = c(20, 30), trunc_adjusted = FALSE,
    cens_adjusted = 0, mvn_id = "A", stringsAsFactors = FALSE
  )))
  expect_error(
    assert_epidist(.estimates_set_vcov(
      varying, list(A = matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2))
    )),
    "must share their study metadata"
  )
})

test_that("as_epidist_estimates_data errors when a required column is missing", { # nolint: line_length_linter.
  complete <- data.frame(
    study = "A", type = "mean", value = 7.5, n = 100,
    stringsAsFactors = FALSE
  )
  for (col in c("study", "type", "value")) {
    expect_error(
      as_epidist_estimates_data(complete[setdiff(names(complete), col)]), col
    )
  }
})

test_that("as_epidist_estimates_data checks the censoring windows against the grid", { # nolint: line_length_linter.
  base <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(2, 1), n = 120,
    relative_obs_time = 8, trunc_adjusted = FALSE, cens_adjusted = 0,
    pwindow = 1, swindow = 1, stringsAsFactors = FALSE
  )
  expect_no_error(suppressMessages(as_epidist_estimates_data(base)))
  zero <- base
  zero$swindow <- 0
  expect_error(
    suppressMessages(as_epidist_estimates_data(zero)),
    "greater than zero"
  )
  # A secondary window wider than the observation time leaves the study with
  # no grid cell it could have recorded a delay in.
  wide <- base
  wide$swindow <- 12
  expect_error(
    suppressMessages(as_epidist_estimates_data(wide)),
    "at least as large as"
  )
})
