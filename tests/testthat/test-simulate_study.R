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
  est <- suppressMessages(
    simulate_study(study_obs, "A", relative_obs_time = 30)
  )
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
  expect_identical(est$relative_obs_time, c(30, 30))
  expect_identical(est$delay_min, c(0, 0))
  expect_identical(est$growth_rate, c(0, 0))
})

test_that("simulate_study returns the rows a quantile study reports", {
  probs <- c(0.1, 0.5, 0.9)
  est <- suppressMessages(
    simulate_study(
      study_obs, "B",
      report = "quantiles", probs = probs, trunc_adjusted = TRUE
    )
  )
  expect_identical(est$type, rep("quantile", 3))
  expect_identical(est$p, probs)
  expect_true(all(diff(est$value) >= 0))
  expect_true(all(is.na(est$se)))
  expect_length(unique(est$n), 1)
})

test_that("simulate_study returns a mean with a standard error", {
  est <- suppressMessages(
    simulate_study(
      study_obs, "C",
      report = "mean_se", n = 100, relative_obs_time = 30
    )
  )
  expect_identical(nrow(est), 1L)
  expect_identical(est$type, "mean")
  expect_true(is.na(est$n))
  expect_true(is.finite(est$se))
  expect_gt(est$se, 0)
})

test_that("simulate_study returns a multivariate mean and sd", {
  est <- suppressMessages(
    simulate_study(
      study_obs, "D",
      report = "multivariate", n = 500, trunc_adjusted = TRUE
    )
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
  est <- suppressMessages(
    simulate_study(study_obs, "F", n = 50, trunc_adjusted = TRUE)
  )
  expect_identical(est$n, c(50, 50))
  est <- suppressMessages(
    simulate_study(study_obs, "G", n = 1e6, trunc_adjusted = TRUE)
  )
  expect_identical(est$n, c(2000, 2000))
})

test_that("simulate_study needs the exact event times", {
  no_times <- simulate_exponential_cases(r = 0, sample_size = 50, t = 10) |>
    simulate_secondary(dist = rlnorm, meanlog = 1.8, sdlog = 0.5) |>
    simulate_dates() |>
    as_epidist_linelist_data() |>
    suppressMessages()
  expect_error(
    simulate_study(no_times, "A"),
    "keep_times = TRUE"
  )
  expect_error(simulate_study(data.frame(x = 1), "A"), "linelist")
  expect_error(simulate_study(study_obs, "A"), "finite")
  expect_error(simulate_study(study_obs, "A", cens_adjusted = 5))
  expect_error(simulate_study(study_obs, "A", report = "median"))
  expect_error(
    simulate_study(
      study_obs, "A",
      report = "quantiles", probs = c(0, 0.5), trunc_adjusted = TRUE
    ),
    "strictly between"
  )
})

test_that("simulate_study errors when no case is observed", {
  expect_error(
    simulate_study(study_obs, "A", trunc_adjusted = TRUE, delay_min = 1000),
    "No case"
  )
})

test_that("simulate_study truncates by the study time, not the line list", {
  # An epidist_linelist_data object carries its own obs_time column, which a
  # filter on a bare argument name would pick up in place of the study's
  # observation time. A naive cohort study cut at 12 days on a line list
  # observed at 120 days must still lose its long delays.
  set.seed(17)
  cases <- simulate_exponential_cases(r = 0, sample_size = 5000, t = 60) |>
    simulate_secondary(dist = rlnorm, meanlog = 1.8, sdlog = 0.5) |>
    simulate_dates(keep_times = TRUE, obs_time = 120)
  linelist <- suppressMessages(as_epidist_linelist_data(cases))
  expect_true(all(linelist$obs_time == 120))
  truncated <- suppressMessages(
    simulate_study(linelist, "A", relative_obs_time = 12)
  )
  full <- suppressMessages(simulate_study(linelist, "A", trunc_adjusted = TRUE))
  expect_lt(truncated$n[1], full$n[1])
  expect_lt(truncated$value[1], full$value[1] - 0.5)
  expect_lte(
    max(linelist$stime_lwr - linelist$ptime_lwr) + 1, 120
  )
  # The same holds for an accrual study and for delay_min.
  accrual <- suppressMessages(simulate_study(
    linelist, "A",
    trunc_design = "accrual", relative_obs_time = 12
  ))
  expect_lt(accrual$n[1], full$n[1])
  above <- suppressMessages(
    simulate_study(linelist, "A", trunc_adjusted = TRUE, delay_min = 5)
  )
  expect_lt(above$n[1], full$n[1])
  expect_gt(above$value[1], full$value[1])
})

# The slots of one simulated study, as .meta_implied_moments() and
# .meta_implied_prob() take them.
study_slots <- function(est) {
  first <- tibble::as_tibble(unclass(est))[1, ]
  return(list(
    lower = first$delay_min,
    cutoff = .estimates_grid_cutoff(first),
    pwindow = first$pwindow,
    swindow = first$swindow,
    trunc_adjusted = as.integer(first$trunc_adjusted),
    cens_adjusted = first$cens_adjusted,
    growth_rate = first$growth_rate,
    trunc_design = .meta_trunc_design(first$trunc_design)
  ))
}

implied_moments <- function(est, args) {
  return(do.call(
    .meta_implied_moments, c(list("plnorm", args), study_slots(est))
  ))
}

implied_prob <- function(y, est, args) {
  return(do.call(
    .meta_implied_prob, c(list(y, "plnorm", args), study_slots(est))
  ))
}

# A reported summary sits within k standard errors of its reference.
expect_within <- function(reported, reference, se, k = 3, slack = 0) {
  return(expect_lte(abs(reported - reference), k * se + slack))
}

# The studies whose summaries are checked against the estimand the meta
# model uses for them. The cohort line list has uniform primary events, so
# its cohort and truncation adjusted studies carry no tilt. The accrual line
# list grows at 0.1 per day over a 24 day window, which is the follow up the
# accrual weight describes.
true_args <- list(meanlog = 1.8, sdlog = 0.5)
study_designs <- data.frame(
  linelist = c(rep("cohort", 6), rep("accrual", 6), rep("cohort", 6)),
  cens_adjusted = c(0, 1, 2, 3, 4, 0, 0, 1, 2, 3, 4, 0, 0, 1, 2, 3, 4, 3),
  trunc_adjusted = c(rep(FALSE, 12), rep(TRUE, 6)),
  trunc_design = c(rep("cohort", 6), rep("accrual", 6), rep("cohort", 6)),
  relative_obs_time = c(rep(20, 6), rep(24, 6), rep(Inf, 6)),
  delay_min = c(0, 0, 0, 2, 2, 3, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2),
  growth_rate = c(rep(0, 6), rep(0.1, 6), rep(0, 6)),
  stringsAsFactors = FALSE
)

design_label <- function(design) {
  return(paste(
    "code", design$cens_adjusted, design$trunc_design,
    ifelse(design$trunc_adjusted, "adjusted", "truncated"),
    "delay_min", design$delay_min
  ))
}

simulate_design <- function(design, linelists, report, probs = NULL) {
  return(suppressMessages(simulate_study(
    linelists[[design$linelist]],
    study = design_label(design),
    report = report,
    probs = probs,
    cens_adjusted = design$cens_adjusted,
    trunc_adjusted = design$trunc_adjusted,
    trunc_design = design$trunc_design,
    relative_obs_time = design$relative_obs_time,
    delay_min = design$delay_min,
    growth_rate = design$growth_rate,
    max_delay = 200
  )))
}

test_that("simulate_study reproduces the naive cohort fixture", {
  skip_on_cran()
  set.seed(12)
  large <- study_linelist(2e5)
  # The fixture of setup.R: integer date differences of a daily line list,
  # keeping a delay when the whole day it falls in is below the cutoff.
  ptime <- runif(1e6)
  delay <- rlnorm(1e6, 1.8, 0.5)
  obs <- floor(ptime + delay)
  for (study_obs_time in c(12, 20)) {
    reference <- obs[obs + 1 <= study_obs_time]
    est <- suppressMessages(simulate_study(
      large, "naive",
      report = "moments", relative_obs_time = study_obs_time
    ))
    n <- est$n[1]
    kurtosis <- mean((reference - mean(reference))^4) / var(reference)^2
    expect_within(
      est$value[1], mean(reference),
      sqrt(var(reference) / n + var(reference) / length(reference))
    )
    expect_within(
      est$value[2], sd(reference),
      sd(reference) * sqrt((kurtosis - 1) / (4 * n))
    )
    quantiles <- suppressMessages(simulate_study(
      large, "naive",
      report = "quantiles", probs = 0.9, relative_obs_time = study_obs_time
    ))
    expect_within(quantiles$value, quantile(reference, 0.9), 0, slack = 1)
  }
})

test_that("simulate_study reproduces the accrual fixture", {
  skip_on_cran()
  set.seed(13)
  # The fixture of setup.R: primary events growing over a collection window,
  # kept when the delay completed before its calendar end, under integer
  # date differences, the uniform single interval approximation and
  # midpoint imputation.
  reference_accrual <- function(size, window, growth_rate, cens) {
    u <- runif(size)
    ptime <- if (growth_rate == 0) {
      u * window
    } else {
      log1p(u * expm1(growth_rate * window)) / growth_rate
    }
    delay <- rlnorm(size, 1.8, 0.5)
    keep <- ptime + delay <= window
    ptime <- ptime[keep]
    delay <- delay[keep]
    return(switch(as.character(cens),
      "0" = floor(ptime + delay) - floor(ptime),
      "2" = ptime - floor(ptime) + delay,
      "3" = floor(ptime + delay) - floor(ptime) + 0.5
    ))
  }
  designs <- list(c(16, 0, 0), c(24, 0.15, 3), c(20, 0, 2))
  for (design in designs) {
    window <- design[1]
    rate <- design[2]
    cens <- design[3]
    large <- study_linelist(2e5, r = rate, t = window)
    reference <- reference_accrual(1e6, window, rate, cens)
    est <- suppressMessages(simulate_study(
      large, "accrual",
      report = "moments", cens_adjusted = cens, trunc_design = "accrual",
      relative_obs_time = window, growth_rate = rate
    ))
    n <- est$n[1]
    kurtosis <- mean((reference - mean(reference))^4) / var(reference)^2
    expect_within(
      est$value[1], mean(reference),
      sqrt(var(reference) / n + var(reference) / length(reference))
    )
    expect_within(
      est$value[2], sd(reference),
      sd(reference) * sqrt((kurtosis - 1) / (4 * n))
    )
    median <- suppressMessages(simulate_study(
      large, "accrual",
      report = "quantiles", probs = 0.5, cens_adjusted = cens,
      trunc_design = "accrual", relative_obs_time = window,
      growth_rate = rate
    ))
    expect_within(
      median$value, median(reference),
      1.2533 * sd(reference) / sqrt(n), slack = as.numeric(cens != 2)
    )
  }
})

test_that("simulate_study moments agree with the implied moments", {
  skip_on_cran()
  set.seed(14)
  linelists <- list(
    cohort = study_linelist(2e5),
    accrual = study_linelist(2e5, r = 0.1, t = 24)
  )
  # The z score of each reported moment against the implied moment at the
  # true parameters, with the sampling error the model would use for it.
  z <- lapply(seq_len(nrow(study_designs)), function(i) {
    design <- study_designs[i, ]
    est <- simulate_design(design, linelists, "moments")
    implied <- implied_moments(est, true_args)
    n <- est$n[1]
    return(c(
      (est$value[1] - implied[["mean"]]) / (implied[["sd"]] / sqrt(n)),
      (est$value[2] - implied[["sd"]]) / .meta_sd_se(implied, n)
    ))
  })
  z <- stats::setNames(
    do.call(rbind, z), c("mean", "sd")
  )
  rownames(z) <- vapply(
    seq_len(nrow(study_designs)),
    function(i) design_label(study_designs[i, ]),
    character(1)
  )
  # Every design agrees within two standard errors. The designs built from
  # the same line list share its cases, so their z scores move together and
  # a run is judged design by design rather than in aggregate.
  expect_lt(max(abs(z)), 2)
})

test_that("simulate_study quantiles agree with the implied probabilities", {
  skip_on_cran()
  set.seed(15)
  linelists <- list(
    cohort = study_linelist(2e5),
    accrual = study_linelist(2e5, r = 0.1, t = 24)
  )
  probs <- c(0.25, 0.5, 0.9)
  z <- lapply(seq_len(nrow(study_designs)), function(i) {
    design <- study_designs[i, ]
    est <- simulate_design(design, linelists, "quantiles", probs)
    n <- est$n[1]
    se <- sqrt(probs * (1 - probs) / n)
    on_grid <- design$cens_adjusted %in% c(0, 3)
    return(vapply(seq_along(probs), function(j) {
      if (on_grid) {
        # A quantile of integer day delays is the cell where the empirical
        # distribution function crosses p, so the implied distribution
        # function at the edges of that cell brackets p, and the z score is
        # the distance to the nearer edge where it does not.
        lower <- implied_prob(est$value[j] - 0.5, est, true_args)
        upper <- implied_prob(est$value[j] + 0.5, est, true_args)
        return(
          (max(lower - probs[j], 0) + min(upper - probs[j], 0)) / se[j]
        )
      }
      return((implied_prob(est$value[j], est, true_args) - probs[j]) / se[j])
    }, numeric(1)))
  })
  z <- do.call(rbind, z)
  rownames(z) <- vapply(
    seq_len(nrow(study_designs)),
    function(i) design_label(study_designs[i, ]),
    character(1)
  )
  expect_lt(max(abs(z)), 2)
})

test_that("simulate_study standard errors agree with the implied ones", {
  skip_on_cran()
  set.seed(16)
  linelist <- study_linelist(1e5)
  for (cens_adjusted in c(0, 2)) {
    est <- suppressMessages(simulate_study(
      linelist, "mvn",
      report = "multivariate", cens_adjusted = cens_adjusted,
      relative_obs_time = 20, n = 20000
    ))
    implied <- implied_moments(est, true_args)
    vcov <- .estimates_vcov(est)[["mvn"]]
    expect_within(est$value[1], implied[["mean"]], sqrt(vcov[1, 1]))
    expect_within(est$value[2], implied[["sd"]], sqrt(vcov[2, 2]))
    # The bootstrap covariance agrees with the sampling variances the model
    # derives from the implied moments.
    expect_equal(
      sqrt(vcov[1, 1]), implied[["sd"]] / sqrt(20000),
      tolerance = 0.1
    )
    expect_equal(
      sqrt(vcov[2, 2]), .meta_sd_se(implied, 20000),
      tolerance = 0.2
    )
    mean_se <- suppressMessages(simulate_study(
      linelist, "mean_se",
      report = "mean_se", cens_adjusted = cens_adjusted,
      relative_obs_time = 20, n = 20000
    ))
    expect_within(mean_se$value, implied[["mean"]], mean_se$se)
    expect_equal(
      mean_se$se, implied[["sd"]] / sqrt(20000),
      tolerance = 0.05
    )
  }
})
