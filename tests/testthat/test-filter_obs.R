test_that("obs_cens_trunc has the correct number of rows", {
  expect_equal(
    sum(obs_cens$stime_upr <= obs_time),
    nrow(obs_cens_trunc)
  )
})
