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
  bad$cens_adjusted[1] <- 3
  expect_error(
    suppressMessages(as_epidist_estimates_data(bad)),
    "cens_adjusted"
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
    "plnorm", args, .estimates_grid_cutoff(est)[1], 1, 1, 1L, 0L, 0
  )
  at_wide <- .meta_implied_moments(
    "plnorm", args, 2000, 1, 1, 1L, 0L, 0
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
