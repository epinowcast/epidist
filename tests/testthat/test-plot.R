test_that("plot_events returns a ggplot with the event windows", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2]]$geom, "GeomSegment")
  # The default draws every case up to the limit, in primary event order
  expect_identical(nrow(p$data), min(nrow(sim_obs), 200L))
  expect_false(is.unsorted(p$data$primary_lwr))
  expect_identical(p$labels$x, "Event time")
  expect_identical(p$labels$colour, "Event")
})

test_that("plot_events limits the number of cases drawn", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs, n = 50)
  expect_identical(nrow(p$data), 50L)
  # The cases kept are spread evenly over the primary event order
  expect_identical(p$data$case[1], 1L)
  expect_identical(p$data$case[50], nrow(sim_obs))
  expect_identical(nrow(p$layers[[2]]$data), 100L)
  p_all <- plot_events(sim_obs, n = Inf)
  expect_identical(nrow(p_all$data), nrow(sim_obs))
})

test_that("plot_events adds an observation time line and a grouping colour", {
  skip_if_not_installed("ggplot2")
  p <- plot_events(sim_obs_sex, obs_time = 20, by = "sex")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 3)
  expect_s3_class(p$layers[[3]]$geom, "GeomVline")
  expect_identical(p$layers[[3]]$data$xintercept, 20)
  expect_identical(p$labels$colour, "sex")
  expect_s3_class(p$layers[[2]]$data$sex, "factor")
})

test_that("plot_events uses dates when the data has them", {
  skip_if_not_installed("ggplot2")
  linelist <- suppressMessages(as_epidist_linelist_data(
    sierra_leone_ebola_data,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ))
  p <- plot_events(linelist, obs_time = as.Date("2014-10-01"), n = 20)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$data$primary_lwr, "Date")
  expect_identical(p$labels$x, "Event date")
  expect_error(plot_events(linelist, obs_time = 10), "must be a date")
})

test_that("plot_events errors on bad input", {
  skip_if_not_installed("ggplot2")
  expect_error(plot_events(data.frame(x = 1)), "epidist_linelist_data")
  expect_error(plot_events(sim_obs, by = "missing"), "missing")
  expect_error(plot_events(sim_obs, by = 1), "string")
  expect_error(plot_events(sim_obs, n = 0), "not >= 1")
  expect_error(plot_events(sim_obs, obs_time = "a"), "obs_time")
})

test_that("plot functions error when ggplot2 is not installed", {
  with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base",
    {
      expect_error(.check_ggplot2(), "ggplot2.*needed to plot")
      expect_error(plot_events(sim_obs), "install.packages")
    }
  )
  expect_null(.check_ggplot2())
})

# Draws of a lognormal delay with an optional second stratum, built as
# delay_parameter_draws() builds them
fake_delay_draws <- function(n = 100, strata = FALSE) {
  set.seed(1)
  draws <- tibble::tibble(
    .row = 1L,
    .draw = seq_len(n),
    mu = rnorm(n, 1.8, 0.05),
    sigma = exp(rnorm(n, log(0.5), 0.05))
  )
  vars <- NULL
  if (strata) {
    draws <- dplyr::bind_rows(
      dplyr::mutate(draws, sex = 0),
      dplyr::mutate(draws, sex = 1, .row = 2L, mu = mu + 0.3)
    )
    vars <- "sex"
  }
  draws <- dplyr::group_by(draws, dplyr::across(dplyr::all_of(c(vars, ".row"))))
  return(.new_delay_draws(draws, .delay_family(brms::lognormal()), vars))
}

test_that("plot.epidist_delay_draws plots the parameter posteriors", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  p <- plot(draws)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomDensity")
  expect_s3_class(p$facet, "FacetWrap")
  expect_identical(levels(p$data$parameter), c("mu", "sigma"))
  expect_identical(nrow(p$data), 200L)
  # Summary columns are plotted too, and pars picks columns
  summaries <- add_summaries(draws, probs = 0.5)
  expect_identical(
    levels(plot(summaries)$data$parameter),
    c("mu", "sigma", "mean", "sd", "q50")
  )
  p_mean <- plot(summaries, pars = "mean")
  expect_identical(levels(p_mean$data$parameter), "mean")
  expect_identical(plot(draws), ggplot2::autoplot(draws))
})

test_that("plot.epidist_delay_draws marks true values", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  p <- plot(draws, true_values = c(mu = 1.8, sigma = 0.5))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[2]]$geom, "GeomVline")
  expect_identical(p$layers[[2]]$data$value, c(1.8, 0.5))
  expect_identical(as.character(p$layers[[2]]$data$parameter), c("mu", "sigma"))
  expect_error(plot(draws, true_values = c(mean = 7)), "mean")
  expect_error(plot(draws, true_values = c(1.8, 0.5)), "names")
})

test_that("plot.epidist_delay_draws colours the strata", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws(strata = TRUE)
  p <- plot(draws)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(levels(p$data$.stratum), c("0", "1"))
  expect_true("sex" %in% names(p$data))
  expect_identical(p$labels$fill, "sex")
  # Draws with several rows but no recorded variables use .row
  attr(draws, "epidist_vars") <- NULL
  expect_identical(plot(draws)$labels$fill, ".row")
  # by overrides the recorded variables
  p_by <- plot(draws, by = c("sex", ".row"))
  expect_identical(levels(p_by$data$.stratum), c("0, 1", "1, 2"))
  expect_identical(p_by$labels$fill, "sex, .row")
  expect_error(plot(draws, by = "missing"), "missing")
  # A single stratum has no legend
  expect_null(plot(fake_delay_draws())$labels$fill)
})

test_that("plot.epidist_delay_draws plots the delay distribution", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  p <- plot(draws, type = "delay")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")
  expect_s3_class(p$layers[[2]]$geom, "GeomLine")
  expect_identical(p$labels$x, "Delay")
  expect_identical(nrow(p$data), 101L)
  expect_identical(min(p$data$delay), 0)
  expect_true(all(p$data$lower <= p$data$density))
  expect_true(all(p$data$density <= p$data$upper))
  # The median density is that of a lognormal and integrates to about one
  step <- diff(p$data$delay[1:2])
  expect_equal(sum(p$data$density) * step, 1, tolerance = 0.02)
  expect_equal(
    max(p$data$delay),
    stats::median(stats::qlnorm(0.99, draws$mu, draws$sigma)),
    tolerance = 1e-8
  )
  p_max <- plot(draws, type = "delay", max_delay = 10)
  expect_identical(max(p_max$data$delay), 10)
})

test_that("plot.epidist_delay_draws draws the delay distribution per draw", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws(strata = TRUE)
  p <- plot(draws, type = "delay", ndraws = 10)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomLine")
  expect_identical(nrow(p$data), as.integer(2 * 10 * 101))
  expect_length(unique(p$data$.draw[p$data$.stratum == "0"]), 10)
  expect_length(unique(p$data$.draw[p$data$.stratum == "1"]), 10)
  expect_identical(levels(p$data$.stratum), c("0", "1"))
  expect_identical(p$labels$colour, "sex")
  # Asking for more draws than there are plots them all
  p_all <- plot(draws, type = "delay", ndraws = 500)
  expect_length(unique(p_all$data$.draw), 100)
})

test_that("plot.epidist_delay_draws simulates for a family with no density", {
  skip_if_not_installed("ggplot2")
  set.seed(1)
  draws <- .new_delay_draws(
    data.frame(mu = rnorm(20, 5, 0.1)),
    .delay_family(brms::brmsfamily("exponential"))
  )
  p <- plot(draws, type = "delay")
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), 101L)
  expect_true(all(p$data$density >= 0))
  # The exponential density is largest at short delays
  expect_gt(p$data$density[2], p$data$density[101])
})

test_that("plot.epidist_delay_draws takes the family from its argument", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  attr(draws, "epidist_family") <- NULL
  expect_error(plot(draws), "Could not work out")
  expect_s3_class(plot(draws, family = "lognormal"), "ggplot")
  expect_error(
    plot(draws, type = "delay", family = "gamma"),
    "missing distributional parameters"
  )
})

test_that("plot.epidist_delay_draws errors on bad arguments", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  expect_error(plot(draws, type = "bogus"), "parameters.*delay")
  expect_error(plot(draws, pars = "missing"), "missing")
  expect_error(plot(draws, type = "delay", probs = 0.5), "length 2")
  expect_error(plot(draws, type = "delay", probs = c(0.9, 0.1)), "sorted")
  expect_error(plot(draws, type = "delay", ndraws = 0), ">= 1")
  expect_error(plot(draws, type = "delay", max_delay = -1), "not >= 0")
})

test_that("plot.epidist_delay_draws colours the delay density by stratum", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws(strata = TRUE)
  p <- plot(draws, type = "delay")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")
  expect_s3_class(p$layers[[2]]$geom, "GeomLine")
  expect_identical(nrow(p$data), 202L)
  expect_identical(levels(p$data$.stratum), c("0", "1"))
  expect_identical(p$labels$colour, "sex")
  expect_identical(p$labels$fill, "sex")
  expect_true(all(p$data$lower <= p$data$density))
  expect_true(all(p$data$density <= p$data$upper))
  # Both strata share the delay grid and the second has longer delays
  expect_identical(p$data$delay[p$data$sex == 0], p$data$delay[p$data$sex == 1])
  peak <- function(sex) {
    stratum <- p$data[p$data$sex == sex, ]
    return(stratum$delay[which.max(stratum$density)])
  }
  expect_lt(peak(0), peak(1))
})

test_that("plot.epidist_delay_draws draws one line per draw of one stratum", {
  skip_if_not_installed("ggplot2")
  draws <- fake_delay_draws()
  p <- plot(draws, type = "delay", ndraws = 10)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomLine")
  expect_identical(nrow(p$data), 1010L)
  expect_length(unique(p$data$.draw), 10)
  expect_true(all(p$data$.draw %in% draws$.draw))
  expect_identical(levels(p$data$.stratum), "all")
  expect_null(p$labels$colour)
  expect_false("lower" %in% names(p$data))
})

test_that("plot.epidist_delay_draws uses the gamma and Weibull densities", {
  skip_if_not_installed("ggplot2")
  # Identical draws make the median density the density of the family
  draws <- data.frame(mu = rep(6, 5), shape = rep(2, 5))
  gamma_draws <- .new_delay_draws(
    draws,
    .delay_family(brms::brmsfamily("gamma"))
  )
  p <- plot(gamma_draws, type = "delay")
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), 101L)
  expect_identical(
    p$data$density,
    stats::dgamma(p$data$delay, shape = 2, rate = 2 / 6)
  )
  expect_equal(
    max(p$data$delay),
    stats::qgamma(0.99, shape = 2, rate = 2 / 6),
    tolerance = 1e-8
  )
  weibull_draws <- .new_delay_draws(draws, .delay_family(brms::weibull()))
  p <- plot(weibull_draws, type = "delay")
  expect_s3_class(p, "ggplot")
  scale <- 6 / gamma(1 + 1 / 2)
  expect_identical(
    p$data$density,
    stats::dweibull(p$data$delay, shape = 2, scale = scale)
  )
  expect_equal(
    max(p$data$delay),
    stats::qweibull(0.99, shape = 2, scale = scale),
    tolerance = 1e-8
  )
})

test_that("plot.epidist_delay_draws errors when no parameter can be plotted", {
  skip_if_not_installed("ggplot2")
  draws <- .new_delay_draws(
    data.frame(.draw = 1:10, x = rnorm(10)),
    .delay_family(brms::lognormal())
  )
  expect_error(plot(draws), "none of the parameters")
  expect_error(plot(draws), "lognormal")
  expect_error(plot(draws), "mu")
  expect_s3_class(plot(draws, pars = "x"), "ggplot")
})
