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
  msgs <- capture_messages(as_epidist_estimates_data(minimal))
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
  est <- suppressMessages(as_epidist_estimates_data(minimal))
  expect_true(est$trunc_adjusted)
  expect_identical(est$relative_obs_time, Inf)
})

test_that("as_epidist_estimates_data errors on an unsupported summary type", {
  bad <- est_df
  bad$type[1] <- "median"
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "type"
  )
})

test_that("as_epidist_estimates_data errors when quantile rows have no probability", { # nolint: line_length_linter.
  bad <- est_df
  bad$p[3] <- NA
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "quantile"
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
    "relative_obs_time"
  )
})

test_that("as_epidist_estimates_data errors on an unsupported adjustment code", { # nolint: line_length_linter.
  bad <- est_df
  bad$cens_adjusted[1] <- 4
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "cens_adjusted"
  )
})

test_that("as_epidist_estimates_data accepts midpoint imputation as a censoring adjustment", { # nolint: line_length_linter.
  mid <- est_df
  mid$cens_adjusted[1] <- 3
  est <- suppressMessages(as_epidist_estimates_data(mid))
  expect_identical(est$cens_adjusted[1], 3L)
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
    "trunc_design"
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
    trunc_adjusted = rep(FALSE, 3), relative_obs_time = rep(20, 3),
    max_delay = rep(100, 3), swindow = rep(2, 3), cens_adjusted = c(0L, 1L, 3L)
  )
  expect_identical(.estimates_quantile_limit(data), c(19, 20, 20))
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

test_that("as_epidist_estimates_data checks a reported covariance matrix", {
  base <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(7.5, 3.6),
    relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0,
    stringsAsFactors = FALSE
  )
  good <- matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2)
  expect_no_error(
    suppressMessages(as_epidist_estimates_data(base, vcov = list(A = good)))
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(base, vcov = list(B = good))),
    "not among the studies"
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(
      base,
      vcov = list(A = matrix(1, nrow = 1))
    )),
    "reports 2 summaries"
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(
      base,
      vcov = list(A = matrix(c(0.4, 0.1, 0.2, 0.25), nrow = 2))
    )),
    "must be symmetric"
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(
      base,
      vcov = list(A = matrix(c(0.4, 0.5, 0.5, 0.25), nrow = 2))
    )),
    "positive definite"
  )
  with_se <- base
  with_se$se <- 0.2
  expect_error(
    suppressMessages(as_epidist_estimates_data(with_se, vcov = list(A = good))),
    "must not also report"
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(base, vcov = list(good))),
    "named by study"
  )
})

test_that("bootstrap_delay_estimates returns summaries with their covariance", {
  set.seed(11)
  delays <- rlnorm(300, 1.6, 0.5)
  reported <- bootstrap_delay_estimates(
    delays,
    study = "A", probs = c(0.25, 0.75), n_bootstrap = 400,
    cens_adjusted = 1
  )
  expect_identical(reported$data$type, c("mean", "sd", "quantile", "quantile"))
  expect_identical(reported$data$p, c(NA, NA, 0.25, 0.75))
  expect_identical(reported$data$value[1], mean(delays))
  expect_identical(reported$data$value[2], stats::sd(delays))
  expect_true(all(reported$data$cens_adjusted == 1))
  expect_named(reported$vcov, "A")
  expect_identical(dim(reported$vcov$A), c(4L, 4L))
  # A sample mean has a variance of about sigma squared over n.
  expect_equal(
    reported$vcov$A[1, 1], stats::var(delays) / length(delays),
    tolerance = 0.25
  )
  expect_no_error(suppressMessages(as_epidist_estimates_data(
    reported$data,
    vcov = reported$vcov
  )))
})

test_that("bootstrap_delay_estimates rejects a bootstrap too small to be full rank", { # nolint: line_length_linter.
  expect_error(
    bootstrap_delay_estimates(rlnorm(50), study = "A", n_bootstrap = 2),
    "must exceed the 2 summaries"
  )
  expect_error(
    bootstrap_delay_estimates(
      rlnorm(50),
      study = "A", moments = character(0)
    ),
    "at least one of"
  )
})

test_that("as_epidist_estimates_data rejects a covariance matrix over summaries with different metadata", { # nolint: line_length_linter.
  varying <- data.frame(
    study = "A", type = c("mean", "sd"), value = c(7.5, 3.6),
    relative_obs_time = c(20, 30), trunc_adjusted = FALSE,
    cens_adjusted = 0, stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(as_epidist_estimates_data(
      varying,
      vcov = list(A = matrix(c(0.4, 0.1, 0.1, 0.25), nrow = 2))
    )),
    "must share their study metadata"
  )
})
