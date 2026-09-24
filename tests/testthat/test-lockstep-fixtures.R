test_that("lockstep_studies() namespaces labels by their fixture", {
  expect_identical(
    lockstep_studies("partial_window", c("A", "A", "B")),
    c("partial_window_A", "partial_window_A", "partial_window_B")
  )
  expect_identical(lockstep_studies("mvn_q", "Q"), "mvn_q_Q")
})

test_that("check_lockstep_studies() accepts the assembled fixtures", {
  expect_identical(
    check_lockstep_studies(lockstep_fixtures),
    unname(lockstep_fixtures)
  )
})

test_that("check_lockstep_studies() fails on a label shared by two fixtures", {
  # Two branches that each append a fixture under the same name merge
  # cleanly, so the collision has to fail here rather than silently join two
  # studies into one.
  shared <- stats::setNames(
    list(
      data.frame(study = c("one_A", "one_A"), stringsAsFactors = FALSE),
      data.frame(study = "one_A", stringsAsFactors = FALSE)
    ),
    c("one", "one")
  )
  expect_error(check_lockstep_studies(shared), "one_A")
})

test_that("check_lockstep_studies() fails on a label outside its fixture", {
  # A fixture that labels its studies by hand leaves its own namespace, which
  # is how a collision becomes possible again.
  by_hand <- list(
    one = data.frame(study = "one_A", stringsAsFactors = FALSE),
    two = data.frame(study = c("AH", "AI"), stringsAsFactors = FALSE)
  )
  expect_error(check_lockstep_studies(by_hand), "AH")
})

test_that("check_lockstep_studies() requires fixtures to be named", {
  unnamed <- list(data.frame(study = "one_A", stringsAsFactors = FALSE))
  expect_error(check_lockstep_studies(unnamed), "named")
})
