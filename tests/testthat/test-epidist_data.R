test_that("epidist objects carry the epidist_data class", {
  expect_true(is_epidist_data(sim_obs))
  expect_true(is_epidist_data(agg_sim_obs))
  expect_true(is_epidist_data(prep_obs))
  expect_true(is_epidist_data(prep_marginal_obs))
  expect_true(is_epidist_data(prep_naive_obs))
  expect_true(is_epidist_data(prep_meta_obs))
  expect_false(is_epidist_data(data.frame()))
})

test_that("epidist_data is placed after the specific epidist classes", {
  expect_s3_class(
    prep_marginal_obs,
    c(
      "epidist_marginal_model",
      "epidist_linelist_data",
      "epidist_data",
      "tbl_df",
      "tbl",
      "data.frame"
    ),
    exact = TRUE
  )
})

test_that("dplyr verbs that keep the required columns keep the class", {
  expect_s3_class(
    dplyr::filter(sim_obs, ptime_lwr > 0),
    "epidist_linelist_data"
  )
  expect_s3_class(dplyr::mutate(sim_obs, extra = 1), "epidist_linelist_data")
  expect_s3_class(dplyr::slice(sim_obs, 1:10), "epidist_linelist_data")
  expect_s3_class(dplyr::arrange(sim_obs, ptime_lwr), "epidist_linelist_data")
})

test_that("dplyr verbs that drop a required column drop the class", {
  expect_warning(dplyr::select(sim_obs, -"obs_time"), "Dropping the")
  expect_warning(
    expect_warning(dplyr::mutate(sim_obs, ptime_lwr = -1), "Dropping the"),
    NA
  )
  dropped <- suppressWarnings(dplyr::select(sim_obs, -"obs_time"))
  expect_false(is_epidist_linelist_data(dropped))
  expect_false(is_epidist_data(dropped))
  expect_s3_class(dropped, "tbl_df")
})

test_that("the warning says which requirement the object no longer meets", {
  expect_warning(
    dplyr::select(sim_obs, -"obs_time"),
    "obs_time"
  )
})

test_that("only the classes whose requirements fail are dropped", {
  expect_warning(
    dplyr::select(prep_marginal_obs, -"n"),
    "epidist_marginal_model"
  )
  dropped <- suppressWarnings(dplyr::select(prep_marginal_obs, -"n"))
  expect_false(is_epidist_marginal_model(dropped))
  expect_true(is_epidist_linelist_data(dropped))
  expect_true(is_epidist_data(dropped))
})

test_that("subsetting rows keeps the class and columns drops it", {
  expect_s3_class(sim_obs[1:10, ], "epidist_linelist_data")
  expect_warning(sim_obs[, 1], "Dropping the")
  expect_false(is_epidist_data(suppressWarnings(sim_obs[, 1])))
})

test_that("replacement functions check the object", {
  data <- sim_obs
  expect_warning(
    {
      names(data)[1] <- "not_ptime_lwr"
    },
    "Dropping the"
  )
  expect_false(is_epidist_data(data))

  data <- sim_obs
  expect_warning(
    {
      data$ptime_lwr <- -1
    },
    "Dropping the"
  )
  expect_false(is_epidist_data(data))

  data <- sim_obs
  expect_warning(
    {
      data[["ptime_lwr"]] <- -1
    },
    "Dropping the"
  )
  expect_false(is_epidist_data(data))

  data <- sim_obs
  expect_warning(
    {
      data[, "ptime_lwr"] <- -1
    },
    "Dropping the"
  )
  expect_false(is_epidist_data(data))

  data <- sim_obs
  data$extra <- 1
  expect_true(is_epidist_linelist_data(data))
})

test_that("modifications that change nothing are not checked", {
  expect_silent(sim_obs[])
})

test_that("binding columns keeps the class without warning", {
  expect_silent(dplyr::bind_cols(sim_obs, tibble::tibble(extra = 1)))
  expect_s3_class(
    dplyr::bind_cols(sim_obs, tibble::tibble(extra = 1)),
    "epidist_linelist_data"
  )
})

test_that("binding rows checks the combined object", {
  bad_row <- tibble::tibble(
    ptime_lwr = -1,
    ptime_upr = 0,
    stime_lwr = 1,
    stime_upr = 2,
    obs_time = 3
  )

  expect_s3_class(rbind(sim_obs, sim_obs), "epidist_linelist_data")
  expect_warning(rbind(sim_obs, bad_row), "Dropping the")
  expect_false(is_epidist_data(suppressWarnings(rbind(sim_obs, bad_row))))

  expect_s3_class(dplyr::bind_rows(sim_obs, sim_obs), "epidist_linelist_data")
  expect_warning(dplyr::bind_rows(sim_obs, bad_row), "Dropping the")
  expect_false(
    is_epidist_data(suppressWarnings(dplyr::bind_rows(sim_obs, bad_row)))
  )
})

test_that("a zero column result is unclassed without a warning", {
  expect_silent(dplyr::select(sim_obs, character(0)))
  expect_false(is_epidist_data(dplyr::select(sim_obs, character(0))))
  expect_silent(sim_obs[0])
  expect_false(is_epidist_data(sim_obs[0]))
})

test_that("dplyr::group_by() keeps the class ahead of grouped_df", {
  grouped <- dplyr::group_by(sim_obs, obs_time)
  expect_s3_class(
    grouped,
    c(
      "epidist_linelist_data",
      "epidist_data",
      "grouped_df",
      "tbl_df",
      "tbl",
      "data.frame"
    ),
    exact = TRUE
  )
  expect_identical(dplyr::group_vars(grouped), "obs_time")

  grouped <- dplyr::group_by(prep_marginal_obs, obs_time)
  expect_s3_class(
    grouped,
    c(
      "epidist_marginal_model",
      "epidist_linelist_data",
      "epidist_data",
      "grouped_df",
      "tbl_df",
      "tbl",
      "data.frame"
    ),
    exact = TRUE
  )
})

test_that("dplyr::ungroup() returns an object of the original class", {
  ungrouped <- sim_obs |>
    dplyr::group_by(obs_time) |>
    dplyr::mutate(extra = 1) |>
    dplyr::ungroup()
  expect_s3_class(ungrouped, class(sim_obs), exact = TRUE)
  expect_identical(dplyr::group_vars(ungrouped), character(0))
  expect_true("extra" %in% names(ungrouped))

  partly <- sim_obs |>
    dplyr::group_by(obs_time, ptime_lwr) |>
    dplyr::ungroup(ptime_lwr)
  expect_s3_class(partly, "epidist_linelist_data")
  expect_s3_class(partly, "grouped_df")
  expect_identical(dplyr::group_vars(partly), "obs_time")

  expect_silent(dplyr::ungroup(sim_obs))
  expect_s3_class(dplyr::ungroup(sim_obs), class(sim_obs), exact = TRUE)
})

test_that("dplyr verbs on a grouped object keep both sets of classes", {
  grouped <- dplyr::group_by(sim_obs, obs_time)
  for (out in list(
    dplyr::mutate(grouped, extra = 1),
    dplyr::filter(grouped, ptime_lwr > 0),
    dplyr::arrange(grouped, ptime_lwr),
    dplyr::slice(grouped, 1:10)
  )) {
    expect_s3_class(out, class(grouped), exact = TRUE)
    expect_identical(dplyr::group_vars(out), "obs_time")
  }
})

test_that("grouped results that fail the requirements drop the class", {
  grouped <- dplyr::group_by(sim_obs, obs_time)
  expect_warning(dplyr::select(grouped, -"ptime_lwr"), "Dropping the")
  dropped <- suppressWarnings(dplyr::select(grouped, -"ptime_lwr"))
  expect_false(is_epidist_data(dropped))
  expect_s3_class(dropped, "grouped_df")

  expect_warning(dplyr::mutate(grouped, ptime_lwr = -1), "Dropping the")
  dropped <- suppressWarnings(dplyr::mutate(grouped, ptime_lwr = -1))
  expect_false(is_epidist_data(dropped))
  expect_s3_class(dropped, "grouped_df")

  expect_warning(
    {
      grouped$ptime_lwr <- -1
    },
    "Dropping the"
  )
  expect_false(is_epidist_data(grouped))
  expect_s3_class(grouped, "grouped_df")
})

test_that("dplyr::summarise() builds a new object without the class", {
  grouped <- dplyr::group_by(sim_obs, obs_time)
  expect_silent(dplyr::summarise(grouped, n = dplyr::n()))
  summary <- dplyr::summarise(grouped, n = dplyr::n())
  expect_false(is_epidist_data(summary))
  expect_s3_class(summary, "tbl_df")
  expect_identical(nrow(summary), 1L)
})

test_that(".new_epidist_data adds epidist_data once", {
  data <- tibble::tibble() |>
    new_epidist_linelist_data() |>
    new_epidist_marginal_model()
  expect_s3_class(
    data,
    c(
      "epidist_marginal_model",
      "epidist_linelist_data",
      "epidist_data",
      "tbl_df",
      "tbl",
      "data.frame"
    ),
    exact = TRUE
  )
})

test_that(".drop_epidist_class keeps epidist_data while a class remains", {
  dropped <- .drop_epidist_class(prep_marginal_obs, "epidist_marginal_model")
  expect_true(is_epidist_data(dropped))
  expect_false(is_epidist_data(.drop_epidist_class(prep_marginal_obs)))
})

test_that("converting a hand built linelist checks it first", {
  # `new_epidist_linelist_data()` does not check, so an invalid object can
  # carry the class until something checks it
  invalid <- new_epidist_linelist_data(tibble::tibble(
    ptime_lwr = 1,
    ptime_upr = 1,
    stime_lwr = 2,
    stime_upr = 3,
    obs_time = 4
  ))

  expect_error(as_epidist_latent_model(invalid))
  expect_error(as_epidist_marginal_model(invalid))
  expect_error(as_epidist_naive_model(invalid))
})
