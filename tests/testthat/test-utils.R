test_that("floor_mult works as expected, including with one and zero as f", { # nolint: line_length_linter.
  expect_equal(floor_mult(1.5, 0.2), 1.4)
  expect_equal(floor_mult(1.5, 1), floor(1.5))
  expect_equal(floor_mult(1.5, 0), 1.5)
  expect_error(floor_mult(1.5, -1))
})
