delay_draws <- function(family = "lognormal", vars = "sex", grouped = TRUE) {
  draws <- tibble::tibble(
    sex = rep(c("m", "f"), each = 2),
    .row = rep(c(1L, 2L), each = 2),
    mu = c(1.8, 1.9, 2.0, 2.1),
    sigma = c(0.5, 0.4, 0.6, 0.3)
  )
  if (grouped) {
    draws <- dplyr::group_by(draws, sex, .row)
  }
  delay <- list(name = family, dpars = c("mu", "sigma"))
  return(.new_delay_draws(draws, delay, vars))
}

expect_delay_draws <- function(object, family = "lognormal", vars = "sex") {
  expect_s3_class(object, "epidist_delay_draws")
  expect_identical(attr(object, "epidist_family")$name, family)
  expect_identical(attr(object, "epidist_vars"), vars)
  return(invisible(object))
}

test_that("dplyr::bind_rows() keeps the class and what it records", {
  draws <- delay_draws()
  out <- dplyr::bind_rows(draws, draws)
  expect_delay_draws(out)
  expect_identical(nrow(out), 8L)
  expect_s3_class(out, "grouped_df")
  # Ungrouped draws keep the class as well
  ungrouped <- delay_draws(grouped = FALSE)
  out <- dplyr::bind_rows(ungrouped, ungrouped)
  expect_delay_draws(out)
  expect_false(inherits(out, "grouped_df"))
})

test_that("plot() dispatches on combined draws", {
  skip_if_not_installed("ggplot2")
  draws <- dplyr::bind_rows(
    dplyr::mutate(delay_draws(), model = "a"),
    dplyr::mutate(delay_draws(), model = "b")
  )
  expect_delay_draws(draws)
  expect_s3_class(plot(draws), "ggplot")
  expect_s3_class(plot(draws, by = "model"), "ggplot")
})

test_that("dplyr::mutate() keeps the class and what it records", {
  out <- dplyr::mutate(delay_draws(), model = "a")
  expect_delay_draws(out)
  expect_s3_class(out, "grouped_df")
  expect_identical(out$model, rep("a", 4))
  out <- dplyr::mutate(delay_draws(grouped = FALSE), model = "a")
  expect_delay_draws(out)
})

test_that("the row slicing verbs keep the class and what it records", {
  draws <- delay_draws()
  filtered <- dplyr::filter(draws, mu > 1.85)
  expect_delay_draws(filtered)
  expect_identical(filtered$mu, c(1.9, 2.0, 2.1))
  expect_delay_draws(dplyr::slice(draws, 1))
  expect_delay_draws(dplyr::arrange(draws, mu))
  expect_identical(dplyr::arrange(draws, dplyr::desc(mu))$mu[1], 2.1)
})

test_that("dplyr::group_by() and dplyr::ungroup() keep the class", {
  draws <- delay_draws()
  ungrouped <- dplyr::ungroup(draws)
  expect_delay_draws(ungrouped)
  expect_false(inherits(ungrouped, "grouped_df"))
  regrouped <- dplyr::group_by(ungrouped, sex)
  expect_delay_draws(regrouped)
  expect_identical(dplyr::group_vars(regrouped), "sex")
  # The class comes before `grouped_df` so that it dispatches first
  expect_identical(class(regrouped)[1], "epidist_delay_draws")
})

test_that("subsetting and selecting keep the class", {
  draws <- delay_draws()
  expect_delay_draws(draws[1:2, ])
  expect_delay_draws(dplyr::select(draws, sex, .row, mu, sigma))
})

test_that("dplyr::bind_rows() keeps the record of its first argument", {
  # `dplyr` restores the class from the first argument alone, so combining
  # draws from different families describes them by the first family
  out <- dplyr::bind_rows(delay_draws(), delay_draws(family = "gamma"))
  expect_delay_draws(out, family = "lognormal")
  out <- dplyr::bind_rows(delay_draws(vars = "age"), delay_draws())
  expect_delay_draws(out, vars = "age")
})

test_that("add_summaries() is no longer needed to keep the class", {
  draws <- dplyr::mutate(delay_draws(), model = "a")
  out <- add_summaries(draws, probs = 0.5)
  expect_delay_draws(out)
  expect_named(
    out,
    c("sex", ".row", "mu", "sigma", "model", "mean", "sd", "q50")
  )
})
