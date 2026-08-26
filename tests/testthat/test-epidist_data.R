test_that("epidist objects carry the epidist_data class", {
  expect_true(is_epidist_data(sim_obs))
  expect_true(is_epidist_data(agg_sim_obs))
  expect_true(is_epidist_data(prep_obs))
  expect_true(is_epidist_data(prep_marginal_obs))
  expect_true(is_epidist_data(prep_naive_obs))
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
  expect_silent(dplyr::bind_cols(sim_obs, tibble::tibble(extra = 1)))
})

test_that("a zero column result is unclassed without a warning", {
  expect_silent(dplyr::select(sim_obs, character(0)))
  expect_false(is_epidist_data(dplyr::select(sim_obs, character(0))))
  expect_silent(sim_obs[0])
  expect_false(is_epidist_data(sim_obs[0]))
})

test_that("dplyr::group_by() drops the class, see epidist issue 629", {
  grouped <- dplyr::group_by(sim_obs, obs_time)
  expect_false(is_epidist_data(grouped))
  expect_false(is_epidist_linelist_data(grouped))
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
