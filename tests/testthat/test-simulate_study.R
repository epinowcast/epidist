# A line list of exact event times, censored to the day, with the times
# kept so that a study can be simulated from it. Primary events are uniform
# over the window, so the estimands of a cohort study have no tilt.
study_linelist <- function(n, r = 0, t = 60, meanlog = 1.8, sdlog = 0.5) {
  cases <- simulate_exponential_cases(r = r, sample_size = n, t = t) |>
    simulate_secondary(dist = rlnorm, meanlog = meanlog, sdlog = sdlog) |>
    simulate_dates(keep_times = TRUE)
  return(suppressMessages(as_epidist_linelist_data(cases)))
}

set.seed(11)
study_obs <- study_linelist(2000)

test_that("simulate_study returns the rows a moments study reports", {
  est <- suppressMessages(simulate_study(study_obs, "A"))
  expect_s3_class(est, "epidist_estimates_data")
  expect_identical(est$study, c("A", "A"))
  expect_identical(est$type, c("mean", "sd"))
  expect_true(all(is.na(est$se)))
  expect_true(all(is.na(est$p)))
  expect_identical(est$n[1], est$n[2])
  expect_lte(est$n[1], 2000)
  expect_identical(est$pwindow, c(1, 1))
  expect_identical(est$swindow, c(1, 1))
  expect_identical(est$cens_adjusted, c(0L, 0L))
  expect_identical(est$trunc_adjusted, c(FALSE, FALSE))
  expect_identical(est$trunc_design, c("cohort", "cohort"))
  expect_identical(est$relative_obs_time, c(Inf, Inf))
  expect_identical(est$delay_min, c(0, 0))
  expect_identical(est$growth_rate, c(0, 0))
})

test_that("simulate_study returns the rows a quantile study reports", {
  probs <- c(0.1, 0.5, 0.9)
  est <- suppressMessages(
    simulate_study(study_obs, "B", report = "quantiles", probs = probs)
  )
  expect_identical(est$type, rep("quantile", 3))
  expect_identical(est$p, probs)
  expect_true(all(diff(est$value) >= 0))
  expect_true(all(is.na(est$se)))
  expect_length(unique(est$n), 1)
})

test_that("simulate_study returns a mean with a standard error", {
  est <- suppressMessages(
    simulate_study(study_obs, "C", report = "mean_se", n = 100)
  )
  expect_identical(nrow(est), 1L)
  expect_identical(est$type, "mean")
  expect_true(is.na(est$n))
  expect_true(is.finite(est$se))
  expect_gt(est$se, 0)
})

test_that("simulate_study returns a multivariate mean and sd", {
  est <- suppressMessages(
    simulate_study(study_obs, "D", report = "multivariate", n = 500)
  )
  expect_identical(est$type, c("mean", "sd"))
  expect_identical(est$mvn_id, c("D", "D"))
  expect_true(all(is.na(est$n)))
  vcov <- .estimates_vcov(est)[["D"]]
  expect_identical(dim(vcov), c(2L, 2L))
  expect_gt(vcov[1, 1], 0)
  expect_gt(vcov[2, 2], 0)
  # The bootstrap standard error of the mean is close to sd / sqrt(n).
  expect_equal(sqrt(vcov[1, 1]), est$value[2] / sqrt(500), tolerance = 0.15)
})

test_that("simulate_study passes the study metadata through", {
  est <- suppressMessages(simulate_study(
    study_obs, "E",
    cens_adjusted = 3, trunc_adjusted = FALSE, trunc_design = "accrual",
    relative_obs_time = 30, delay_min = 2, growth_rate = 0.1,
    max_delay = 45, site = "north"
  ))
  expect_identical(est$cens_adjusted, c(3L, 3L))
  expect_identical(est$trunc_design, c("accrual", "accrual"))
  expect_identical(est$relative_obs_time, c(30, 30))
  expect_identical(est$delay_min, c(2, 2))
  expect_identical(est$growth_rate, c(0.1, 0.1))
  expect_identical(est$max_delay, c(45, 45))
  expect_identical(est$site, c("north", "north"))
  # A subsample of the available cases, and never more than there are.
  est <- suppressMessages(simulate_study(study_obs, "F", n = 50))
  expect_identical(est$n, c(50, 50))
  est <- suppressMessages(simulate_study(study_obs, "G", n = 1e6))
  expect_identical(est$n, c(2000, 2000))
})

test_that("simulate_study needs the exact event times", {
  no_times <- suppressMessages(as_epidist_linelist_data(
    simulate_exponential_cases(r = 0, sample_size = 50, t = 10) |>
      simulate_secondary(dist = rlnorm, meanlog = 1.8, sdlog = 0.5) |>
      simulate_dates()
  ))
  expect_error(
    simulate_study(no_times, "A"),
    "keep_times = TRUE"
  )
  expect_error(simulate_study(data.frame(x = 1), "A"), "linelist")
  expect_error(simulate_study(study_obs, "A", cens_adjusted = 5))
  expect_error(simulate_study(study_obs, "A", report = "median"))
  expect_error(
    simulate_study(study_obs, "A", report = "quantiles", probs = c(0, 0.5)),
    "strictly between"
  )
})

test_that("simulate_study errors when no case is observed", {
  expect_error(
    simulate_study(study_obs, "A", delay_min = 1000),
    "No case"
  )
})
