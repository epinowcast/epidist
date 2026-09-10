# fmt: skip file
# A table with the columns epireview uses. A 2015 reports a mean and a
# standard deviation, B 2016 a median with an interquartile range and a mean
# with a standard error, C 2017 a gamma fit recorded as a mean and a standard
# deviation, D 2018 no value, E 2019 a value type that is not a mean or a
# median, and F 2020 an inverse rate.
epireview_df <- data.frame(
  article_label = c(
    "A 2015", "B 2016", "B 2016", "C 2017", "D 2018", "E 2019", "F 2020"
  ),
  covidence_id = c(1L, 2L, 2L, 3L, 4L, 5L, 6L),
  parameter_type = "Human delay - Symptom Onset to Death",
  parameter_value = c(10.6, 14, 8.9, 9.5, NA, 12, 0.1),
  parameter_value_type = c(
    "Mean", "Median", "Mean", "Mean", "Mean", "Other", "Mean"
  ),
  parameter_unit = "Days",
  exponent = 0L,
  inverse_param = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  parameter_uncertainty_single_value = c(3.2, NA, 0.7, NA, NA, NA, NA),
  parameter_uncertainty_singe_type = c(
    "Standard Deviation", NA, "Standard Error", NA, NA, NA, NA
  ),
  parameter_uncertainty_lower_value = c(NA, 11, NA, NA, NA, NA, NA),
  parameter_uncertainty_upper_value = c(NA, 15, NA, NA, NA, NA, NA),
  parameter_uncertainty_type = c(NA, "IQR", NA, NA, NA, NA, NA),
  distribution_type = c(NA, NA, NA, "Gamma", NA, NA, NA),
  distribution_par1_value = c(NA, NA, NA, 9.5, NA, NA, NA),
  distribution_par1_type = c(NA, NA, NA, "Mean", NA, NA, NA),
  distribution_par2_value = c(NA, NA, NA, 4.4, NA, NA, NA),
  distribution_par2_type = c(NA, NA, NA, "Mean sd", NA, NA, NA),
  population_sample_size = c(76, 20, 92, 40, 15, 30, 50),
  method_moment_value = c(
    "Post outbreak", "Mid outbreak", "Mid outbreak", NA, NA, NA, NA
  ),
  stringsAsFactors = FALSE
)

quiet_epireview <- function(...) {
  return(suppressWarnings(suppressMessages(epidist_estimates_epireview(...))))
}

test_that("epidist_estimates_epireview maps means, medians and their spreads", { # nolint: line_length_linter.
  estimates <- quiet_epireview(
    epireview_df,
    trunc_adjusted = TRUE, cens_adjusted = 0
  )
  expect_s3_class(estimates, "epidist_estimates_data")
  expect_s3_class(estimates, "epidist_data")
  expect_identical(
    estimates$study,
    c(
      "A 2015", "A 2015", "B 2016", "B 2016", "B 2016", "B 2016", "C 2017",
      "C 2017"
    )
  )
  expect_identical(
    estimates$type,
    c("mean", "sd", "quantile", "quantile", "quantile", "mean", "mean", "sd")
  )
  expect_identical(estimates$value, c(10.6, 3.2, 14, 11, 15, 8.9, 9.5, 4.4))
  expect_identical(estimates$p, c(NA, NA, 0.5, 0.25, 0.75, NA, NA, NA))
  expect_identical(estimates$se, c(NA, NA, NA, NA, NA, 0.7, NA, NA))
  expect_identical(estimates$n, c(76, 76, 20, 20, 20, 92, 40, 40))
  expect_true(all(estimates$trunc_adjusted))
  expect_true(all(estimates$cens_adjusted == 0L))
})

test_that("epidist_estimates_epireview drops the records it cannot map with a message", { # nolint: line_length_linter.
  messages <- capture_messages(
    suppressWarnings(epidist_estimates_epireview(epireview_df))
  )
  dropped <- messages[grepl("Dropped 3 records", messages, fixed = TRUE)]
  expect_length(dropped, 1)
  expect_match(dropped, "\"F 2020\" \\(row 7\\) reports an inverse rate")
  expect_match(
    dropped, "\"D 2018\" \\(row 5\\), \"E 2019\" \\(row 6\\) report no mean"
  )
})

test_that("epidist_estimates_epireview drops records in other units, with an exponent, or with no uncertainty", { # nolint: line_length_linter.
  other <- epireview_df[1:3, ]
  other$parameter_unit[1] <- "Weeks"
  other$exponent[2] <- 1L
  other$population_sample_size[3] <- NA
  other$parameter_uncertainty_singe_type[3] <- NA
  expect_message(
    expect_error(epidist_estimates_epireview(other), "No record"),
    "units other than days"
  )
  expect_message(
    expect_error(epidist_estimates_epireview(other), "No record"),
    "a scaling exponent"
  )
  expect_message(
    expect_error(epidist_estimates_epireview(other), "No record"),
    "no sample size and no standard error"
  )
  # A mean with a standard error needs no sample size
  other$parameter_uncertainty_singe_type[3] <- "Standard Error"
  estimates <- quiet_epireview(other, trunc_adjusted = TRUE)
  expect_identical(estimates$study, "B 2016")
  expect_identical(estimates$se, 0.7)
})

test_that("epidist_estimates_epireview takes a standard error from a confidence interval of a mean", { # nolint: line_length_linter.
  ci <- epireview_df[1, ]
  ci$parameter_uncertainty_single_value <- NA
  ci$parameter_uncertainty_singe_type <- NA
  ci$parameter_uncertainty_lower_value <- 9
  ci$parameter_uncertainty_upper_value <- 12
  ci$parameter_uncertainty_type <- "95% CI"
  estimates <- quiet_epireview(ci, trunc_adjusted = TRUE)
  expect_identical(estimates$type, "mean")
  expect_equal(estimates$se, 3 / (2 * qnorm(0.975)), tolerance = 1e-8)
})

test_that("epidist_estimates_epireview ignores a spread that does not match the value type", { # nolint: line_length_linter.
  mismatched <- epireview_df[2, ]
  mismatched$parameter_uncertainty_single_value <- 2.4
  mismatched$parameter_uncertainty_singe_type <- "Standard Deviation"
  mismatched$parameter_uncertainty_type <- "Range"
  estimates <- quiet_epireview(mismatched, trunc_adjusted = TRUE)
  expect_identical(estimates$type, "quantile")
  expect_identical(estimates$value, 14)
})

test_that("epidist_estimates_epireview converts reported distribution parameters", { # nolint: line_length_linter.
  fitted <- epireview_df[c(1, 1, 1, 1), ]
  fitted$article_label <- c("gamma", "weibull", "lognormal", "rate")
  fitted$parameter_uncertainty_single_value <- NA
  fitted$parameter_uncertainty_singe_type <- NA
  fitted$distribution_type <- c("Gamma", "Weibull", "Normal-Log", "Gamma")
  fitted$distribution_par1_type <- c("Shape", "Scale", "Meanlog", "Shape")
  fitted$distribution_par1_value <- c(4, 2, 1.6, 4)
  fitted$distribution_par2_type <- c("Scale", "Shape", "Sdlog", "Rate")
  fitted$distribution_par2_value <- c(2, 1.5, 0.5, 0.5)
  estimates <- quiet_epireview(fitted, trunc_adjusted = TRUE, cens_adjusted = 1)
  expect_identical(estimates$type, rep(c("mean", "sd"), 4))
  # The reported value is not used where the parameters are
  expect_identical(estimates$value[c(1, 2)], c(8, 4))
  expect_identical(estimates$value[c(7, 8)], c(8, 4))
  expect_equal(
    estimates$value[c(3, 4)],
    c(2 * gamma(1 + 1 / 1.5), 2 * sqrt(gamma(1 + 2 / 1.5) - gamma(1 + 1 / 1.5)^2)), # nolint: line_length_linter.
    tolerance = 1e-8
  )
  expect_equal(
    estimates$value[c(5, 6)],
    c(exp(1.6 + 0.5^2 / 2), exp(1.6 + 0.5^2 / 2) * sqrt(expm1(0.5^2))),
    tolerance = 1e-8
  )
  expect_identical(estimates$n, rep(76, 8))
})

test_that("epidist_estimates_epireview takes the implied summaries over the delays the study saw", { # nolint: line_length_linter.
  fitted <- epireview_df[1, ]
  fitted$distribution_type <- "Gamma"
  fitted$distribution_par1_type <- "Shape"
  fitted$distribution_par1_value <- 4
  fitted$distribution_par2_type <- "Scale"
  fitted$distribution_par2_value <- 2
  truncated <- quiet_epireview(
    fitted,
    relative_obs_time = 12, trunc_adjusted = FALSE
  )
  reference <- suppressWarnings(suppressMessages(epidist_estimates_parameters(
    "A 2015", "gamma", c(shape = 4, scale = 2),
    n = 76, relative_obs_time = 12, trunc_adjusted = FALSE
  )))
  expect_identical(truncated$value, reference$value)
  expect_lt(truncated$value[1], 8)
})

test_that("epidist_estimates_epireview falls back to the reported summaries for an unsupported parameterisation", { # nolint: line_length_linter.
  fitted <- epireview_df[1, ]
  fitted$distribution_type <- "Normal-Log"
  fitted$distribution_par1_type <- "Mean"
  fitted$distribution_par1_value <- 2
  fitted$distribution_par2_type <- "Variance"
  fitted$distribution_par2_value <- 0.3
  estimates <- quiet_epireview(fitted, trunc_adjusted = TRUE)
  expect_identical(estimates$type, c("mean", "sd"))
  expect_identical(estimates$value, c(10.6, 3.2))
})

test_that("epidist_estimates_epireview treats a standard deviation of zero as not reported", { # nolint: line_length_linter.
  zero <- epireview_df[1, ]
  zero$parameter_uncertainty_single_value <- 0
  estimates <- quiet_epireview(zero, trunc_adjusted = TRUE)
  expect_identical(estimates$type, "mean")
})

test_that("epidist_estimates_epireview applies shared metadata to every record", { # nolint: line_length_linter.
  estimates <- quiet_epireview(
    epireview_df,
    pwindow = 7, swindow = 7, relative_obs_time = 60, trunc_adjusted = FALSE,
    trunc_design = "accrual", cens_adjusted = 2, delay_min = 1,
    growth_rate = 0.1
  )
  expect_true(all(estimates$pwindow == 7))
  expect_true(all(estimates$swindow == 7))
  expect_true(all(estimates$relative_obs_time == 60))
  expect_false(any(estimates$trunc_adjusted))
  expect_true(all(estimates$trunc_design == "accrual"))
  expect_true(all(estimates$cens_adjusted == 2L))
  expect_true(all(estimates$delay_min == 1))
  expect_true(all(estimates$growth_rate == 0.1))
})

test_that("epidist_estimates_epireview checks the shared metadata", {
  expect_error(
    epidist_estimates_epireview(epireview_df, n = 10),
    "subset of"
  )
  expect_error(
    epidist_estimates_epireview(epireview_df, cens_adjusted = c(0, 1)),
    "single value"
  )
})

test_that("epidist_estimates_epireview assumes the metadata it is not given as as_epidist_estimates_data does", { # nolint: line_length_linter.
  expect_warning(
    suppressMessages(epidist_estimates_epireview(epireview_df)),
    "assumed to have adjusted for right truncation"
  )
  expect_message(
    suppressWarnings(epidist_estimates_epireview(epireview_df)),
    "No cens_adjusted column supplied"
  )
  expect_message(
    suppressWarnings(epidist_estimates_epireview(epireview_df)),
    "No `pwindow` column supplied"
  )
  estimates <- quiet_epireview(epireview_df)
  expect_true(all(estimates$trunc_adjusted))
  expect_true(all(estimates$cens_adjusted == 0L))
  expect_true(all(is.infinite(estimates$relative_obs_time)))
})

test_that("epidist_estimates_epireview takes metadata per study", {
  metadata <- data.frame(
    study = c("A 2015", "C 2017"),
    relative_obs_time = c(40, NA),
    trunc_adjusted = c(FALSE, NA),
    cens_adjusted = c(1, 3),
    n = c(NA, 100),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_epireview(
    epireview_df,
    metadata = metadata, trunc_adjusted = TRUE, cens_adjusted = 0
  )
  a <- estimates[estimates$study == "A 2015", ]
  expect_true(all(a$relative_obs_time == 40))
  expect_false(any(a$trunc_adjusted))
  expect_true(all(a$cens_adjusted == 1L))
  expect_true(all(a$n == 76))
  b <- estimates[estimates$study == "B 2016", ]
  expect_true(all(is.infinite(b$relative_obs_time)))
  expect_true(all(b$trunc_adjusted))
  expect_true(all(b$cens_adjusted == 0L))
  c <- estimates[estimates$study == "C 2017", ]
  # An NA leaves the shared value in place
  expect_true(all(c$trunc_adjusted))
  expect_true(all(c$cens_adjusted == 3L))
  expect_true(all(c$n == 100))
})

test_that("epidist_estimates_epireview fills a missing sample size from the metadata", { # nolint: line_length_linter.
  missing_n <- epireview_df[1, ]
  missing_n$population_sample_size <- NA
  expect_error(
    suppressMessages(epidist_estimates_epireview(missing_n)),
    "No record"
  )
  estimates <- quiet_epireview(
    missing_n,
    metadata = data.frame(study = "A 2015", n = 50, stringsAsFactors = FALSE),
    trunc_adjusted = TRUE
  )
  expect_identical(estimates$n, c(50, 50))
})

test_that("epidist_estimates_epireview fills the studies a metadata column leaves blank and says so", { # nolint: line_length_linter.
  metadata <- data.frame(
    study = "A 2015", relative_obs_time = 40, trunc_adjusted = FALSE,
    swindow = 7, stringsAsFactors = FALSE
  )
  expect_message(
    suppressWarnings(
      epidist_estimates_epireview(epireview_df, metadata = metadata)
    ),
    "No swindow given for \"B 2016\" and \"C 2017\", assuming 1"
  )
  expect_message(
    suppressWarnings(
      epidist_estimates_epireview(epireview_df, metadata = metadata)
    ),
    "No relative_obs_time given for \"B 2016\" and \"C 2017\", assuming Inf"
  )
  expect_warning(
    suppressMessages(
      epidist_estimates_epireview(epireview_df, metadata = metadata)
    ),
    "No trunc_adjusted given for \"B 2016\" and \"C 2017\""
  )
  estimates <- quiet_epireview(epireview_df, metadata = metadata)
  a <- estimates$study == "A 2015"
  expect_true(all(estimates$swindow[a] == 7))
  expect_true(all(estimates$swindow[!a] == 1))
  expect_false(any(estimates$trunc_adjusted[a]))
  expect_true(all(estimates$trunc_adjusted[!a]))
  # A study given a finite observation time but no truncation flag is assumed
  # not to have adjusted, with a message rather than a warning
  metadata <- data.frame(
    study = c("A 2015", "B 2016"), relative_obs_time = c(40, NA),
    trunc_adjusted = c(NA, TRUE), stringsAsFactors = FALSE
  )
  expect_message(
    suppressWarnings(
      epidist_estimates_epireview(epireview_df, metadata = metadata)
    ),
    "No trunc_adjusted given for \"A 2015\", so it is assumed not to have"
  )
})

test_that("epidist_estimates_epireview checks the metadata table", {
  expect_error(
    epidist_estimates_epireview(
      epireview_df,
      metadata = data.frame(
        study = "Z 2000", cens_adjusted = 1, stringsAsFactors = FALSE
      )
    ),
    "not a study"
  )
  expect_error(
    epidist_estimates_epireview(
      epireview_df,
      metadata = data.frame(
        study = c("A 2015", "A 2015"), cens_adjusted = 1,
        stringsAsFactors = FALSE
      )
    ),
    "more than once"
  )
  expect_error(
    epidist_estimates_epireview(
      epireview_df,
      metadata = data.frame(
        study = "A 2015", max_delay = 100, stringsAsFactors = FALSE
      )
    ),
    "subset of"
  )
  expect_error(
    epidist_estimates_epireview(
      epireview_df,
      metadata = data.frame(article_label = "A 2015", stringsAsFactors = FALSE)
    ),
    "study"
  )
})

test_that("epidist_estimates_epireview takes another study column and keeps covariates", { # nolint: line_length_linter.
  estimates <- quiet_epireview(
    epireview_df,
    study = "covidence_id", keep = "method_moment_value",
    trunc_adjusted = TRUE
  )
  expect_identical(estimates$study, c("1", "1", "2", "2", "2", "2", "3", "3"))
  expect_identical(
    estimates$method_moment_value,
    c(rep("Post outbreak", 2), rep("Mid outbreak", 4), NA, NA)
  )
  expect_error(
    epidist_estimates_epireview(epireview_df, keep = "phase"),
    "phase"
  )
})

test_that("epidist_estimates_epireview requires one delay and the columns it reads", { # nolint: line_length_linter.
  mixed <- epireview_df
  mixed$parameter_type[1] <- "Human delay - Symptom Onset to Admission"
  expect_error(epidist_estimates_epireview(mixed), "one `parameter_type`")
  expect_error(
    epidist_estimates_epireview(
      epireview_df[, names(epireview_df) != "parameter_value"]
    ),
    "parameter_value"
  )
  expect_error(
    epidist_estimates_epireview(epireview_df, study = "author"),
    "author"
  )
})

test_that("epidist_estimates_epireview returns an object that can be edited afterwards", { # nolint: line_length_linter.
  estimates <- quiet_epireview(epireview_df, trunc_adjusted = TRUE)
  edited <- dplyr::mutate(
    estimates,
    relative_obs_time = ifelse(.data$study == "A 2015", 40, Inf),
    trunc_adjusted = .data$study != "A 2015"
  )
  expect_s3_class(edited, "epidist_estimates_data")
  expect_identical(
    edited$relative_obs_time[edited$study == "A 2015"], c(40, 40)
  )
  # A change that breaks a requirement drops the class
  expect_warning(
    dplyr::mutate(estimates, trunc_adjusted = FALSE),
    "Dropping"
  )
  broken <- suppressWarnings(dplyr::mutate(estimates, trunc_adjusted = FALSE))
  expect_false(is_epidist_estimates_data(broken))
})

test_that("epidist_estimates_epireview maps the epireview Ebola onset to death estimates", { # nolint: line_length_linter.
  skip_if_not_installed("epireview")
  params <- suppressMessages(epireview::load_epidata("ebola"))$params
  onset_to_death <- params[
    params$parameter_type_short == "delay_onset_to_death",
  ]
  expect_message(
    suppressWarnings(epidist_estimates_epireview(
      onset_to_death,
      trunc_adjusted = TRUE, cens_adjusted = 0
    )),
    "Dropped 16 records"
  )
  estimates <- quiet_epireview(
    onset_to_death,
    trunc_adjusted = TRUE, cens_adjusted = 0, keep = "method_moment_value"
  )
  expect_s3_class(estimates, "epidist_estimates_data")
  expect_gt(nrow(estimates), 40)
  expect_true(all(estimates$value > 0))
  expect_true(all(estimates$type %in% c("mean", "sd", "quantile")))
  expect_true(all(!is.na(estimates$n) | !is.na(estimates$se)))
  expect_true(all(estimates$trunc_adjusted))
  expect_true(hasName(estimates, "method_moment_value"))
  # Xu 2016 reports a mean of 8.6 with a standard deviation of 4.8 from 76
  # deaths, and Uyeki 2016 a median of 14 from 5
  xu <- estimates[estimates$study == "Xu 2016", ]
  expect_identical(xu$type, c("mean", "sd"))
  expect_identical(xu$value, c(8.6, 4.8))
  expect_identical(xu$n, c(76, 76))
  uyeki <- estimates[estimates$study == "Uyeki 2016", ]
  expect_identical(uyeki$type, "quantile")
  expect_identical(uyeki$p, 0.5)
})
