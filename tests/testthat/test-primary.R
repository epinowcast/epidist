test_that("every primary event distribution declares what both models need", {
  fields <- c("id", "dpars", "links", "bounds", "ddist", "rdist", "args")
  for (name in .primary_choices()) {
    spec <- .primary_spec(name)
    expect_named(spec, fields, info = name)
    expect_true(checkmate::test_int(spec$id), info = name)
    expect_length(spec$links, length(spec$dpars))
    expect_length(spec$bounds, length(spec$dpars))
    expect_length(spec$args, length(spec$dpars))
    expect_true(is.function(spec$ddist), info = name)
    expect_true(is.function(spec$rdist), info = name)
    # primarycensored supplies the window bounds and the registry the rest.
    expect_true(
      all(c("min", "max", spec$args) %in% names(formals(spec$ddist))),
      info = name
    )
    expect_true(
      all(c("min", "max", spec$args) %in% names(formals(spec$rdist))),
      info = name
    )
  }
})

test_that("the primary event distributions do not collide", {
  specs <- lapply(.primary_choices(), .primary_spec)
  ids <- vapply(specs, function(spec) spec$id, integer(1))
  expect_identical(anyDuplicated(ids), 0L)
  dpars <- unlist(lapply(specs, function(spec) spec$dpars))
  expect_identical(anyDuplicated(dpars), 0L)
})

test_that("the primary event distribution ids are the primarycensored ones", {
  code <- primarycensored::pcd_load_stan_functions("primary_lpdf")
  for (name in .primary_choices()) {
    expect_match(
      code,
      paste0("primary_id == ", .primary_spec(name)$id),
      fixed = TRUE,
      info = name
    )
  }
})

test_that("uniform is the default primary event distribution", {
  expect_identical(.primary_choices()[1], "uniform")
  expect_identical(.primary_dist(data.frame(x = 1)), "uniform")
})

test_that(".primary_spec rejects a distribution that is not implemented", {
  expect_error(.primary_spec("gaussian"))
})

test_that(".add_primary_dpars records the distribution and its parameters", {
  family <- list(
    dpars = c("mu", "sigma"),
    other_links = "log",
    other_bounds = list(list(lb = 0, ub = NA))
  )
  data <- structure(data.frame(x = 1), primary = "expgrowth")
  out <- .add_primary_dpars(family, data)
  expect_identical(out$primary, "expgrowth")
  expect_identical(out$dpars, c("mu", "sigma", "pgrowth"))
  expect_length(out$other_links, 2L)
  expect_length(out$other_bounds, 2L)
})

test_that(".add_primary_dpars adds nothing for a uniform primary event", {
  family <- list(dpars = c("mu", "sigma"), other_links = "log")
  out <- .add_primary_dpars(family, data.frame(x = 1))
  expect_identical(out$primary, "uniform")
  expect_identical(out$dpars, c("mu", "sigma"))
  expect_identical(out$other_links, "log")
})

test_that(".primary_stancode_args passes the declared parameters to Stan", {
  expect_identical(
    .primary_stancode_args(.primary_spec("uniform")),
    "1, primary_params"
  )
  expect_identical(
    .primary_stancode_args(.primary_spec("expgrowth")),
    "2, {pgrowth}"
  )
})

test_that("a family carrying no primary event distribution is uniform", {
  expect_identical(.primary_spec_from_family(list())$id, 1L)
  expect_identical(
    .primary_spec_from_family(list(primary = "expgrowth"))$id,
    2L
  )
})

test_that("the fit decides the primary event distribution, not the family", {
  uniform <- .primary_spec("uniform")
  prep <- list(family = list(primary = "expgrowth"))
  expect_identical(.primary_spec_from_prep(prep, uniform)$id, 2L)
  expect_identical(.primary_spec_from_prep(list(), uniform)$id, 1L)
})

test_that(".primary_args names the arguments the density takes", {
  prep <- structure(
    list(dpars = list(pgrowth = matrix(c(0.1, 0.2), nrow = 2, ncol = 3))),
    class = "brmsprep"
  )
  args <- .primary_args(.primary_spec("expgrowth"), prep, i = 1)
  expect_named(args, "r")
  expect_identical(args$r, c(0.1, 0.2))
  expect_identical(
    .primary_args(.primary_spec("expgrowth"), prep, i = 1, draw = 2)$r,
    0.2
  )
  expect_length(.primary_args(.primary_spec("uniform"), prep, i = 1), 0L)
})
