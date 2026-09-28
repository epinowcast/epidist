#' Plot the primary and secondary event windows of each case
#'
#' @description
#' Draws one row per case, ordered by primary event time, with the primary and
#' secondary event windows as horizontal segments joined by a line for the
#' delay between them. A dashed vertical line marks the observation time when
#' one is given. This is the plot the vignettes use to show how censoring and
#' truncation obscure the delays.
#'
#' @details
#' Dates are used when `data` has the date columns that
#' [as_epidist_linelist_data.data.frame()] keeps, `pdate_lwr` and so on, and
#' the numeric time columns otherwise. `obs_time` must be on the same scale.
#'
#' Cases are ordered by the lower bound of their primary event window and
#' numbered in that order, so the vertical axis shows the growth of the
#' outbreak. When there are more than `n` cases, `n` evenly spaced cases in
#' that order are drawn. This keeps the shape of the outbreak without
#' over-plotting.
#'
#' The plot is drawn with [ggplot2::theme_minimal()] and the colour blind
#' friendly palette the package documentation uses. Add a theme or a scale of
#' your own to the returned plot to override either. The column named by `by`
#' is kept in the plot data, so the plot can be faceted by it.
#'
#' @param data An `epidist_linelist_data` object.
#'
#' @param obs_time The observation time to mark with a dashed vertical line,
#'  as a date or a number on the scale of the event times. If `NULL`, the
#'  default, no line is drawn.
#'
#' @param by A string naming a column of `data` to colour the cases by. If
#'  `NULL`, the default, the primary and secondary event windows are coloured
#'  differently instead.
#'
#' @param n The maximum number of cases to draw. Defaults to 200. Use `Inf` to
#'  draw every case.
#'
#' @family plot
#' @returns A `ggplot` object.
#'
#' @autoglobal
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' linelist <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   )
#' plot_events(linelist, n = 100)
#' plot_events(linelist, obs_time = as.Date("2014-10-01"), by = "district")
plot_events <- function(data, obs_time = NULL, by = NULL, n = 200) {
  .check_ggplot2()
  if (!is_epidist_linelist_data(data)) {
    cli_abort("{.arg data} must be an {.cls epidist_linelist_data} object.")
  }
  assert_string(by, null.ok = TRUE)
  assert_number(n, lower = 1)
  if (!is.null(by)) {
    assert_names(names(data), must.include = by)
  }
  use_dates <- all(.linelist_date_cols() %in% names(data))
  if (use_dates) {
    cols <- .linelist_date_cols()
    if (!is.null(obs_time) && !lubridate::is.timepoint(obs_time)) {
      cli_abort("{.arg obs_time} must be a date, as {.arg data} has dates.")
    }
  } else {
    cols <- .linelist_required_cols()
    assert_number(obs_time, null.ok = TRUE)
  }

  cases <- tibble::tibble(
    primary_lwr = data[[cols[1]]],
    primary_upr = data[[cols[2]]],
    secondary_lwr = data[[cols[3]]],
    secondary_upr = data[[cols[4]]]
  )
  if (!is.null(by)) {
    cases[[by]] <- factor(data[[by]])
  }
  cases <- cases[order(cases$primary_lwr), ]
  cases$case <- seq_len(nrow(cases))
  if (nrow(cases) > n) {
    cases <- cases[round(seq(1, nrow(cases), length.out = n)), ]
  }

  windows <- dplyr::bind_rows(
    primary = dplyr::rename(
      cases[setdiff(names(cases), c("secondary_lwr", "secondary_upr"))],
      lwr = "primary_lwr",
      upr = "primary_upr"
    ),
    secondary = dplyr::rename(
      cases[setdiff(names(cases), c("primary_lwr", "primary_upr"))],
      lwr = "secondary_lwr",
      upr = "secondary_upr"
    ),
    .id = "event"
  )

  p <- ggplot2::ggplot(cases, ggplot2::aes(y = .data$case)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$primary_lwr,
        xend = .data$secondary_upr,
        yend = .data$case
      ),
      colour = "grey70",
      linewidth = 0.4
    )
  if (is.null(by)) {
    p <- p +
      ggplot2::geom_segment(
        data = windows,
        ggplot2::aes(
          x = .data$lwr,
          xend = .data$upr,
          yend = .data$case,
          colour = .data$event
        ),
        linewidth = 1.5,
        lineend = "round"
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          primary = .epidist_palette()[1],
          secondary = .epidist_palette()[2]
        ),
        labels = c(primary = "Primary", secondary = "Secondary")
      ) +
      ggplot2::labs(colour = "Event")
  } else {
    p <- p +
      ggplot2::geom_segment(
        data = windows,
        ggplot2::aes(
          x = .data$lwr,
          xend = .data$upr,
          yend = .data$case,
          colour = .data[[by]]
        ),
        linewidth = 1.5,
        lineend = "round"
      ) +
      .epidist_colour_scale(nlevels(cases[[by]])) +
      ggplot2::labs(colour = by)
  }
  if (!is.null(obs_time)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = obs_time,
        linetype = "dashed",
        colour = "grey30"
      )
  }
  p <- p +
    ggplot2::labs(
      x = ifelse(use_dates, "Event date", "Event time"),
      y = "Case"
    ) +
    .epidist_plot_theme()
  return(p)
}

#' Check that `ggplot2` is installed
#'
#' `ggplot2` is a suggested package, so the plot functions check for it before
#' they use it.
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
.check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort(c(
      "The {.pkg ggplot2} package is needed to plot.",
      i = "Install it with {.code install.packages(\"ggplot2\")}."
    ))
  }
  return(invisible(NULL))
}

#' The colour palette the package plots use
#'
#' The Okabe-Ito palette, ordered so that the first two colours are the blue
#' and green the vignettes use for the primary and secondary events. It is
#' colour blind friendly and holds eight colours.
#'
#' @returns A character vector of colours.
#'
#' @keywords internal
.epidist_palette <- function() {
  return(c(
    "#56B4E9", "#009E73", "#E69F00", "#CC79A7",
    "#0072B2", "#D55E00", "#F0E442", "#999999"
  ))
}

#' The discrete colour scale the package plots use
#'
#' Uses `.epidist_palette()` when it holds enough colours and the viridis
#' scale otherwise. The scale covers both the `colour` and the `fill`
#' aesthetic, so that a variable mapped to each gets one legend.
#'
#' @param n The number of levels to colour.
#'
#' @returns A `ggplot2` scale.
#'
#' @keywords internal
.epidist_colour_scale <- function(n) {
  aesthetics <- c("colour", "fill")
  values <- .epidist_palette()
  if (n <= length(values)) {
    return(ggplot2::scale_colour_manual(
      values = unname(values),
      aesthetics = aesthetics
    ))
  }
  return(ggplot2::scale_colour_viridis_d(end = 0.9, aesthetics = aesthetics))
}

#' The theme the package plots use
#'
#' [ggplot2::theme_minimal()] with the legend below the plot, as the package
#' documentation draws its plots.
#'
#' @returns A `ggplot2` theme.
#'
#' @keywords internal
.epidist_plot_theme <- function() {
  return(
    ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold")
      )
  )
}

#' Plot posterior draws of the delay distribution
#'
#' @description
#' Plots the draws returned by [delay_parameter_draws()],
#' [delay_summary_draws()] and [add_summaries()]. The default,
#' `type = "parameters"`, draws the posterior density of each distributional
#' parameter, and of any summary column [add_summaries()] added, in its own
#' panel. `type = "delay"` draws the delay distribution the draws imply over a
#' grid of delays, as the posterior median density with a ribbon between two
#' quantiles, or as one line per draw when `ndraws` is given.
#'
#' @details
#' The strata of the draws are coloured. By default they are the unique
#' combinations of the variables in the distributional parameter formulas,
#' which [delay_parameter_draws()] records, and each `.row` when the draws
#' have several rows of `newdata` but no such variables. Pass `by` to
#' stratify by other columns of the draws. The columns in `by` are kept in the
#' plot data, so the plot can be faceted by them.
#'
#' The delay distribution is evaluated with the density of the family for the
#' lognormal, gamma and Weibull families. For any other family, delays are
#' simulated from each draw as in [add_summaries()] and their density is
#' estimated with [stats::density()]. Either way the density is evaluated at
#' every draw, so thin the draws with the `ndraws` argument of
#' [delay_parameter_draws()], or build `newdata` with [epidist_strata()],
#' when there are many.
#'
#' `plot()` and `autoplot()` are the same function. Both need `ggplot2`.
#'
#' The plot is drawn with [ggplot2::theme_minimal()] and the colour blind
#' friendly palette the package documentation uses. Add a theme or a scale of
#' your own to the returned plot to override either.
#'
#' @param x,object An `epidist_delay_draws` object, as returned by
#'  [delay_parameter_draws()], [delay_summary_draws()] or [add_summaries()].
#'
#' @param type Either `"parameters"`, the default, to plot the posterior
#'  density of each parameter, or `"delay"` to plot the delay distribution the
#'  draws imply.
#'
#' @param by A character vector of columns of `object` that define the strata
#'  to colour by. If `NULL`, the default, the variables recorded on `object`
#'  are used. See the details.
#'
#' @param pars A character vector of the columns to plot when
#'  `type = "parameters"`. If `NULL`, the default, the distributional
#'  parameters of the family are plotted, along with the `mean`, `sd` and
#'  quantile columns present.
#'
#' @param true_values A named numeric vector of true parameter values to mark
#'  with dashed vertical lines when `type = "parameters"`. The names must be
#'  among the parameters plotted.
#'
#' @param ndraws The number of draws per stratum to plot the delay
#'  distribution of when `type = "delay"`, one line each, sampled at random.
#'  If `NULL`, the default, the posterior median density is drawn with a
#'  ribbon between the `probs` quantiles instead.
#'
#' @param probs A numeric vector of two probabilities giving the quantiles the
#'  ribbon spans when `type = "delay"`. Defaults to `c(0.05, 0.95)`.
#'
#' @param max_delay The largest delay to evaluate the delay distribution at
#'  when `type = "delay"`. If `NULL`, the default, the posterior median of the
#'  99% quantile of the delay distribution is used.
#'
#' @param family A model fit with [epidist::epidist()], a `brms` family, or
#'  the name of one, giving the delay distribution. If `NULL`, the default,
#'  the family recorded on `object` is used.
#'
#' @param ... Passed from `plot()` to `autoplot()`. Unused otherwise.
#'
#' @family plot
#' @returns A `ggplot` object.
#'
#' @seealso [delay_parameter_draws()] and [delay_summary_draws()] for the
#'  draws, and [plot_events()] to plot the data.
#'
#' @method plot epidist_delay_draws
#' @autoglobal
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' draws <- data.frame(
#'   mu = rnorm(200, 1.8, 0.05),
#'   sigma = exp(rnorm(200, log(0.5), 0.05))
#' ) |>
#'   add_summaries(family = "lognormal", probs = 0.5)
#' plot(draws, true_values = c(mu = 1.8, sigma = 0.5))
#' plot(draws, type = "delay")
#' plot(draws, type = "delay", ndraws = 50)
plot.epidist_delay_draws <- function(x, ...) {
  .check_ggplot2()
  return(ggplot2::autoplot(x, ...))
}

#' @rdname plot.epidist_delay_draws
#' @exportS3Method ggplot2::autoplot
autoplot.epidist_delay_draws <- function(
  object,
  type = c("parameters", "delay"),
  by = NULL,
  pars = NULL,
  true_values = NULL,
  ndraws = NULL,
  probs = c(0.05, 0.95),
  max_delay = NULL,
  family = NULL,
  ...
) {
  .check_ggplot2()
  type <- match.arg(type)
  family <- .resolve_delay_family(object, family)
  strata <- .draw_strata(object, by)
  if (identical(type, "parameters")) {
    return(.plot_parameter_draws(strata, family, pars, true_values))
  }
  return(.plot_delay_draws(strata, family, ndraws, probs, max_delay))
}

#' Label the strata of a `data.frame` of draws
#'
#' @inheritParams plot.epidist_delay_draws
#'
#' @returns A list with `data`, an ungrouped `tibble` of the draws with a
#'  `.stratum` factor column, `by`, the columns the strata are defined by or
#'  `NULL` when there is a single stratum, and `legend`, a legend title.
#'
#' @keywords internal
.draw_strata <- function(object, by = NULL) {
  assert_character(by, any.missing = FALSE, null.ok = TRUE)
  draws <- tibble::as_tibble(dplyr::ungroup(object))
  if (is.null(by)) {
    by <- intersect(attr(object, "epidist_vars"), names(draws))
    if (length(by) == 0 && length(unique(draws[[".row"]])) > 1) {
      by <- ".row"
    }
    if (length(by) == 0 || nrow(unique(draws[by])) <= 1) {
      by <- NULL
    }
  } else {
    assert_names(names(draws), must.include = by)
  }
  if (is.null(by)) {
    draws$.stratum <- factor("all")
    return(list(data = draws, by = NULL, legend = NULL))
  }
  draws$.stratum <- factor(do.call(paste, c(draws[by], sep = ", ")))
  return(list(data = draws, by = by, legend = toString(by)))
}

#' Plot the posterior density of each parameter
#'
#' @inheritParams plot.epidist_delay_draws
#'
#' @param strata A list as returned by `.draw_strata()`.
#'
#' @param family A list with the delay distribution `name` and its
#'  distributional parameters `dpars`, as returned by `.delay_family()`.
#'
#' @returns A `ggplot` object.
#'
#' @autoglobal
#' @keywords internal
.plot_parameter_draws <- function(strata, family, pars = NULL,
                                  true_values = NULL) {
  draws <- strata$data
  if (is.null(pars)) {
    quantile_cols <- grep("^q[0-9.]+$", names(draws), value = TRUE)
    pars <- intersect(
      c(family$dpars, "mean", "sd", quantile_cols),
      names(draws)
    )
    if (length(pars) == 0) {
      cli_abort(
        "{.arg object} has none of the parameters of the {.val {family$name}}
         family: {.val {family$dpars}}."
      )
    }
  } else {
    assert_character(pars, any.missing = FALSE, min.len = 1)
    assert_names(names(draws), must.include = pars)
  }
  if (!is.null(true_values)) {
    assert_numeric(true_values, any.missing = FALSE, names = "unique")
    assert_names(names(true_values), subset.of = pars)
  }
  long <- tidyr::pivot_longer(
    draws[c(strata$by, ".stratum", pars)],
    cols = dplyr::all_of(pars),
    names_to = "parameter",
    values_to = "value"
  )
  long$parameter <- factor(long$parameter, levels = pars)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value))
  if (is.null(strata$by)) {
    p <- p +
      ggplot2::geom_density(
        fill = .epidist_palette()[1],
        colour = .epidist_palette()[1],
        alpha = 0.5,
        linewidth = 0.4
      )
  } else {
    p <- p +
      ggplot2::geom_density(
        ggplot2::aes(fill = .data$.stratum, colour = .data$.stratum),
        alpha = 0.5,
        linewidth = 0.4
      ) +
      .epidist_colour_scale(nlevels(long$.stratum)) +
      ggplot2::labs(fill = strata$legend, colour = strata$legend)
  }
  if (!is.null(true_values)) {
    true_data <- tibble::tibble(
      parameter = factor(names(true_values), levels = pars),
      value = unname(true_values)
    )
    p <- p +
      ggplot2::geom_vline(
        data = true_data,
        ggplot2::aes(xintercept = .data$value),
        linetype = "dashed",
        colour = "grey30"
      )
  }
  p <- p +
    ggplot2::facet_wrap(ggplot2::vars(.data$parameter), scales = "free") +
    ggplot2::labs(x = NULL, y = "Posterior density") +
    .epidist_plot_theme()
  return(p)
}

#' Plot the delay distribution the draws imply
#'
#' @inheritParams .plot_parameter_draws
#'
#' @returns A `ggplot` object.
#'
#' @autoglobal
#' @keywords internal
.plot_delay_draws <- function(strata, family, ndraws = NULL,
                              probs = c(0.05, 0.95), max_delay = NULL) {
  assert_count(ndraws, positive = TRUE, null.ok = TRUE)
  assert_numeric(
    probs,
    lower = 0,
    upper = 1,
    len = 2,
    any.missing = FALSE,
    sorted = TRUE
  )
  assert_number(max_delay, lower = 0, null.ok = TRUE)
  draws <- strata$data
  .assert_dpars(draws, family$name, family$dpars)
  dens <- .delay_density_grid(
    family,
    as.list(draws)[family$dpars],
    max_delay = max_delay
  )
  draw_id <- draws[[".draw"]]
  if (is.null(draw_id)) {
    draw_id <- seq_len(nrow(draws))
  }
  keep_cols <- c(strata$by, ".stratum")
  plot_data <- purrr::map(
    split(seq_len(nrow(draws)), draws$.stratum),
    function(rows) {
      if (is.null(ndraws)) {
        values <- dens$density[rows, , drop = FALSE]
        out <- tibble::tibble(
          delay = dens$delays,
          density = apply(values, 2, stats::median),
          lower = apply(values, 2, stats::quantile, probs[1], names = FALSE),
          upper = apply(values, 2, stats::quantile, probs[2], names = FALSE)
        )
      } else {
        rows <- rows[sample.int(length(rows), min(ndraws, length(rows)))]
        values <- dens$density[rows, , drop = FALSE]
        out <- tibble::tibble(
          .draw = rep(draw_id[rows], times = ncol(values)),
          # One line per row of the draws, as a draw is not unique to a row
          .line = rep(rows, times = ncol(values)),
          delay = rep(dens$delays, each = nrow(values)),
          density = as.vector(values)
        )
      }
      return(dplyr::bind_cols(
        out,
        draws[rep(rows[1], nrow(out)), keep_cols, drop = FALSE]
      ))
    }
  ) |>
    purrr::list_rbind()

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$delay))
  if (is.null(ndraws) && is.null(strata$by)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = .epidist_palette()[1],
        alpha = 0.3
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$density),
        colour = .epidist_palette()[1],
        linewidth = 0.8
      )
  } else if (is.null(ndraws)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data$lower,
          ymax = .data$upper,
          fill = .data$.stratum
        ),
        alpha = 0.3
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$density, colour = .data$.stratum),
        linewidth = 0.8
      ) +
      .epidist_colour_scale(nlevels(plot_data$.stratum)) +
      ggplot2::labs(colour = strata$legend, fill = strata$legend)
  } else if (is.null(strata$by)) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$density, group = .data$.line),
        colour = .epidist_palette()[1],
        alpha = 0.2
      )
  } else {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data$density,
          group = .data$.line,
          colour = .data$.stratum
        ),
        alpha = 0.2
      ) +
      .epidist_colour_scale(nlevels(plot_data$.stratum)) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(override.aes = list(alpha = 1))
      ) +
      ggplot2::labs(colour = strata$legend)
  }
  p <- p +
    ggplot2::labs(x = "Delay", y = "Density") +
    .epidist_plot_theme()
  return(p)
}

#' Evaluate the density of the delay distribution at each draw
#'
#' Uses the density of the family when there is an analytic solution, and
#' otherwise simulates delays from each draw with `.simulate_delays()` and
#' estimates their density with [stats::density()].
#'
#' @inheritParams .simulate_delays
#' @inheritParams plot.epidist_delay_draws
#'
#' @param n_grid The number of delays to evaluate the density at.
#'
#' @returns A list with `delays`, the delays the density is evaluated at, and
#'  `density`, a matrix with one row per draw and one column per delay.
#'
#' @keywords internal
.delay_density_grid <- function(family, dpars, max_delay = NULL, n_grid = 101,
                                nsim = 1000) {
  n <- length(dpars[[1]])
  analytic <- .analytic_delay_summaries(family$name)
  if (!is.null(analytic) && all(analytic$dpars %in% names(dpars))) {
    if (is.null(max_delay)) {
      max_delay <- stats::median(analytic$quantile(dpars, 0.99))
    }
    delays <- seq(0, max_delay, length.out = n_grid)
    values <- vapply(
      delays,
      function(x) {
        return(analytic$density(dpars, x))
      },
      numeric(n)
    )
  } else {
    samples <- .simulate_delays(family, dpars, nsim)
    if (is.null(max_delay)) {
      per_draw_q99 <- apply(samples, 1, stats::quantile, probs = 0.99)
      max_delay <- stats::median(per_draw_q99)
    }
    delays <- seq(0, max_delay, length.out = n_grid)
    values <- apply(samples, 1, function(x) {
      return(stats::density(x, from = 0, to = max_delay, n = n_grid)$y)
    })
    values <- t(values)
  }
  return(list(delays = delays, density = matrix(values, nrow = n)))
}
