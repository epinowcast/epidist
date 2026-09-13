test_that("plot_delays returns a ggplot of the binned observed delays", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomCol")
  expect_named(p$data, c(".stratum", "delay", "n", "p", "density"))
  expect_identical(levels(p$data$.stratum), "all")
  delays <- sim_obs$stime_lwr - sim_obs$ptime_lwr
  expect_identical(nrow(p$data), length(unique(delays)))
  expect_identical(sum(p$data$n), as.double(nrow(sim_obs)))
  expect_equal(sum(p$data$p), 1, tolerance = 1e-12)
  expect_equal(p$data$density, p$data$p, tolerance = 1e-12)
  expect_false(is.unsorted(p$data$delay))
  expect_identical(p$labels$x, "Delay")
  expect_identical(p$labels$y, "Density")
})

test_that("plot_delays weights aggregate data by its counts", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs)
  p_agg <- plot_delays(agg_sim_obs)
  expect_identical(p_agg$data$n, p$data$n)
  expect_equal(p_agg$data$p, p$data$p, tolerance = 1e-12)
  expect_identical(sum(p_agg$data$n), as.double(nrow(sim_obs)))
})

test_that("plot_delays bins to the width given", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs, binwidth = 2)
  expect_true(all(p$data$delay %% 2 == 0))
  expect_equal(sum(p$data$p), 1, tolerance = 1e-12)
  expect_equal(p$data$density, p$data$p / 2, tolerance = 1e-12)
  built <- ggplot2::layer_data(p, 1)
  expect_equal(unique(built$xmax - built$xmin), 2, tolerance = 1e-12)
})

test_that("plot_delays strata by a column of the data", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs_sex, by = "sex")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(levels(p$data$.stratum), c("0", "1"))
  expect_identical(p$labels$fill, "sex")
  # The column is kept so that the plot can be faceted by it
  expect_s3_class(p$data$sex, "factor")
  # Proportions are within each stratum
  by_stratum <- as.vector(tapply(p$data$p, p$data$.stratum, sum))
  expect_equal(by_stratum, c(1, 1), tolerance = 1e-12)
})

test_that("plot_delays compares a named list of datasets", {
  skip_if_not_installed("ggplot2")
  truncated <- dplyr::filter(sim_obs, .data$stime_upr <= 15)
  p <- plot_delays(list(All = sim_obs, Truncated = truncated))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(levels(p$data$.stratum), c("All", "Truncated"))
  expect_identical(sum(p$data$n), as.double(nrow(sim_obs) + nrow(truncated)))
  by_stratum <- as.vector(tapply(p$data$p, p$data$.stratum, sum))
  expect_equal(by_stratum, c(1, 1), tolerance = 1e-12)
  expect_null(p$labels$fill)
})

test_that("plot_delays overlays a reference distribution", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs, reference = c(mu = 1.8, sigma = 0.5))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[2]]$geom, "GeomLine")
  line <- p$layers[[2]]$data
  expect_named(line, c("delay", "density"))
  expect_identical(min(line$delay), 0)
  expect_equal(
    line$density,
    stats::dlnorm(line$delay, meanlog = 1.8, sdlog = 0.5),
    tolerance = 1e-8
  )
  # Other families are reached through their distributional parameters
  p_gamma <- plot_delays(
    sim_obs_gamma,
    reference = c(mu = 6, shape = 2),
    family = "gamma"
  )
  expect_no_error(ggplot2::ggplot_build(p_gamma))
  expect_equal(
    p_gamma$layers[[2]]$data$density,
    stats::dgamma(p_gamma$layers[[2]]$data$delay, shape = 2, rate = 2 / 6),
    tolerance = 1e-8
  )
})

test_that("plot_delays marks a minimum delay", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs, delay_min = 3)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[2]]$geom, "GeomVline")
  expect_identical(p$layers[[2]]$data$xintercept, 3)
  # The data carries a minimum delay of its own
  with_min <- dplyr::mutate(sim_obs, delay_min = 2)
  expect_identical(
    plot_delays(with_min)$layers[[2]]$data$xintercept,
    2
  )
})

test_that("plot_delays marks a minimum delay the datasets agree on", {
  skip_if_not_installed("ggplot2")
  truncated <- dplyr::filter(sim_obs, .data$stime_upr <= 15)
  agreed <- list(
    All = dplyr::mutate(sim_obs, delay_min = 2),
    Truncated = dplyr::mutate(truncated, delay_min = 2)
  )
  p <- plot_delays(agreed)
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[2]]$geom, "GeomVline")
  expect_identical(p$layers[[2]]$data$xintercept, 2)
  # Datasets that disagree have no single minimum delay to mark
  disagreed <- agreed
  disagreed$Truncated$delay_min <- 3
  expect_length(plot_delays(disagreed)$layers, 1)
})

test_that("plot_delays uses the package theme and palette", {
  skip_if_not_installed("ggplot2")
  p <- plot_delays(sim_obs)
  expect_identical(p$theme$legend.position, "bottom")
  expect_identical(p$layers[[1]]$aes_params$fill, .epidist_palette()[1])
  p_sex <- plot_delays(sim_obs_sex, by = "sex")
  expect_identical(
    unname(p_sex$scales$get_scales("fill")$palette(2))[1:2],
    .epidist_palette()[1:2]
  )
})

test_that("plot_delays errors on bad input", {
  skip_if_not_installed("ggplot2")
  expect_error(plot_delays(data.frame(x = 1)), "epidist_linelist_data")
  expect_error(plot_delays(list()), "epidist_linelist_data")
  expect_error(plot_delays(list(sim_obs, sim_obs)), "named")
  expect_error(
    plot_delays(list(a = sim_obs), by = "sex"),
    "cannot be used"
  )
  expect_error(plot_delays(sim_obs, by = "missing"), "missing")
  expect_error(plot_delays(sim_obs, binwidth = 0), "not >= ")
  expect_error(plot_delays(sim_obs, delay_min = "a"), "delay_min")
  expect_error(plot_delays(sim_obs, delay_min = -1), "not >= 0")
  expect_error(
    plot_delays(sim_obs, reference = c(mu = 1.8)),
    "sigma"
  )
})

test_that("plot_delays draws the delays a fitted model predicts", {
  skip_if_not_installed("ggplot2")
  skip_if_no_fits()
  p <- plot_delays(fit_marginal, ndraws = 50)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 3)
  expect_s3_class(p$layers[[1]]$geom, "GeomCol")
  expect_s3_class(p$layers[[2]]$geom, "GeomRibbon")
  expect_s3_class(p$layers[[3]]$geom, "GeomLine")
  # The columns are the observed delays of the data the model was fitted to
  observed <- plot_delays(sim_obs)
  expect_identical(p$data$delay, observed$data$delay)
  expect_identical(p$data$n, observed$data$n)
  expect_equal(p$data$p, observed$data$p, tolerance = 1e-12)
  # The predictions are the proportion of the cases in each bin
  predicted <- p$layers[[2]]$data
  expect_named(predicted, c(".stratum", "delay", "density", "lower", "upper"))
  expect_true(all(predicted$lower <= predicted$density))
  expect_true(all(predicted$density <= predicted$upper))
  expect_gt(sum(predicted$density), 0.8)
  expect_lt(sum(predicted$density), 1.05)
  expect_identical(p$labels$x, "Delay")
  expect_identical(p$labels$y, "Density")
})

test_that("plot_delays colours the strata of a fitted model", {
  skip_if_not_installed("ggplot2")
  skip_if_no_fits()
  p <- plot_delays(fit_marginal_sex, ndraws = 50)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(levels(p$data$.stratum), c("0", "1"))
  expect_identical(p$labels$fill, "sex")
  # The column is kept so that the plot can be faceted by it
  expect_true("sex" %in% names(p$data))
  by_stratum <- as.vector(tapply(p$data$p, p$data$.stratum, sum))
  expect_equal(by_stratum, c(1, 1), tolerance = 1e-12)
  # The predictions are stratified too
  expect_identical(levels(p$layers[[2]]$data$.stratum), c("0", "1"))
  # A single stratum can be asked for by name
  p_one <- plot_delays(fit_marginal, by = "pwindow", ndraws = 50)
  expect_identical(p_one$labels$fill, "pwindow")
})

test_that("plot_delays errors on a fitted model it cannot plot", {
  skip_if_not_installed("ggplot2")
  skip_if_no_fits()
  expect_error(plot_delays(fit_marginal, by = "missing"), "missing")
  expect_error(plot_delays(fit_marginal, ndraws = 0), "ndraws")
  expect_error(plot_delays(fit_meta_estimates), "meta analytic")
})

test_that("plot_delays errors when ggplot2 is not installed", {
  with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base",
    expect_error(plot_delays(sim_obs), "install.packages")
  )
})
