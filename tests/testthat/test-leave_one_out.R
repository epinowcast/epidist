test_that(".leave_one_out_studies returns the studies in order of appearance", {
  data <- data.frame(
    study = c("B", "A", "B", "individual"),
    stringsAsFactors = FALSE
  )
  expect_identical(.leave_one_out_studies(data), c("B", "A", "individual"))
})

test_that(".leave_one_out_studies holds individual level rows out as one study", { # nolint: line_length_linter.
  studies <- .leave_one_out_studies(prep_meta_obs)
  expect_identical(studies, c("individual", unique(sim_estimates$study)))
})

test_that(".leave_one_out_studies errors without a study column or with one", {
  expect_error(
    .leave_one_out_studies(prep_meta_individual),
    "no .*study.* column"
  )
  expect_error(
    .leave_one_out_studies(
      data.frame(study = c("A", "A"), stringsAsFactors = FALSE)
    ),
    "only the study"
  )
})

test_that(".leave_one_out_data uses the model data of the fit when it can", {
  fit <- structure(
    list(data = data.frame(study = c("A", "B"), stringsAsFactors = FALSE)),
    class = c("brmsfit", "epidist_fit")
  )
  expect_identical(.leave_one_out_data(fit), fit$data)
  expect_identical(
    .leave_one_out_data(fit, data = prep_meta_estimates),
    prep_meta_estimates
  )
  fit$data <- data.frame(x = 1)
  expect_error(.leave_one_out_data(fit), "no .*study.* column")
  expect_error(
    .leave_one_out_data(
      fit,
      data = data.frame(study = "A", stringsAsFactors = FALSE)
    ),
    "epidist_meta_model"
  )
})

test_that(".assert_meta_fit only accepts meta model fits from epidist()", {
  fit <- structure(
    list(family = list(name = "meta_lognormal")),
    class = c("brmsfit", "epidist_fit")
  )
  expect_invisible(.assert_meta_fit(fit))
  fit$family$name <- "marginal_lognormal"
  expect_error(.assert_meta_fit(fit), "meta model")
  class(fit) <- "brmsfit"
  expect_error(.assert_meta_fit(fit), "epidist")
})

test_that(".drop_study removes a study and keeps the model data valid", {
  family <- epidist_family(prep_meta_obs)
  fit <- list(
    family = family,
    formula = epidist_formula(prep_meta_obs, family, formula = mu ~ 1)
  )
  study <- unique(sim_estimates$study)[1]
  held_out <- .drop_study(prep_meta_obs, study, fit)
  expect_s3_class(held_out, "epidist_meta_model")
  expect_false(study %in% held_out$study)
  expect_identical(
    setdiff(unique(prep_meta_obs$study), study),
    unique(held_out$study)
  )
  # Individual level rows are summarised as they are for a fit
  expect_lt(nrow(held_out), nrow(prep_meta_obs))
  expect_identical(
    sum(held_out$n[held_out$obs_type == 1L]),
    sum(prep_meta_obs$n[prep_meta_obs$obs_type == 1L])
  )
  # The grouped summary members travel with the data
  expect_identical(.meta_members(held_out), .meta_members(prep_meta_obs))
  without_individual <- .drop_study(prep_meta_obs, "individual", fit)
  expect_s3_class(without_individual, "epidist_meta_model")
  expect_true(all(without_individual$obs_type != 1L))
  expect_identical(
    nrow(without_individual),
    sum(prep_meta_obs$obs_type != 1L)
  )
})

test_that(".drop_study subsets the plain model data of a fit directly", {
  data <- data.frame(
    study = c("A", "B", "B"), n = 1:3, stringsAsFactors = FALSE
  )
  held_out <- .drop_study(data, "B", fit = list())
  expect_identical(held_out, data[1, , drop = FALSE])
})

test_that(".leave_one_out_compare flags held out medians outside the interval", { # nolint: line_length_linter.
  full <- tibble::tibble(
    .row = 1L,
    summary = c("mean", "sd"),
    estimate = c(10, 4),
    lower = c(9, 3),
    upper = c(11, 5),
    posterior_sd = c(0.5, 0.5)
  )
  held <- tibble::tibble(
    study = rep(c("A", "B"), each = 2),
    .row = 1L,
    summary = rep(c("mean", "sd"), 2),
    estimate = c(10.5, 4.25, 12, 3.75),
    lower = c(9, 3, 10, 2),
    upper = c(12, 5, 14, 5),
    posterior_sd = c(0.8, 0.6, 1, 0.7)
  )
  out <- .leave_one_out_compare(held, full)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c(
    "study", ".row", "summary", "estimate", "lower", "upper",
    "full_estimate", "full_lower", "full_upper", "shift", "influential"
  ))
  expect_identical(out$study, held$study)
  expect_identical(out$full_estimate, rep(c(10, 4), 2))
  expect_identical(out$full_lower, rep(c(9, 3), 2))
  expect_identical(out$full_upper, rep(c(11, 5), 2))
  expect_identical(out$shift, c(1, 0.5, 4, -0.5))
  expect_identical(out$influential, c(FALSE, FALSE, TRUE, FALSE))
})
