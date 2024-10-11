test_that("add_event_vars produces equivalent linelists in different ways", { # nolint: line_length_linter.
  linelist <- tibble::tibble(
    "a" = runif(100),
    "b" = 1,
    "c" = a + b,
    "d" = runif(100, 2, 3),
    "e" = 1,
    "f" = d + e
  )

  ll <- linelist |>
    add_event_vars(
      ptime_lwr = "a", pwindow = "b", ptime_upr = "c",
      stime_lwr = "d", swindow = "e", stime_upr = "f"
    )

  ll2 <- select(linelist, a, c, d, f) |>
    add_event_vars(
      ptime_lwr = "a", pwindow = 1, ptime_upr = "c",
      stime_lwr = "d", swindow = 1, stime_upr = "f"
    )

  ll3 <- select(linelist, a, b, d, e) |>
    add_event_vars(
      ptime_lwr = "a", pwindow = "b", stime_lwr = "d", swindow = "e",
    )

  ll4 <- select(linelist, a, c, d, f) |>
    add_event_vars(
      ptime_lwr = "a", ptime_upr = "c", stime_lwr = "d", stime_upr = "f",
    )

  expect_equal(ll, ll2)
  expect_equal(ll, ll3)
  expect_equal(ll, ll4)
})
