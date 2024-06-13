#' Plot the posterior estimates as densities
#'
#' @param data ...
#' @param alpha ...
#' @param quantiles ...
#' @param ... ...
#' @family plot
#' @export
plot_recovery <- function(data, alpha = 0.8,
                          quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  data |>
    ggplot() +
    aes(x = value, ...) +
    ggridges::geom_density_ridges(
      scale = 1.5, alpha = alpha, quantile_lines = TRUE,
      quantiles = quantiles
    ) +
    theme_bw()
}

#' Plot the relative difference between true values and posterior estimates
#'
#' @param relative_data ...
#' @param alpha ...
#' @param quantiles ...
#' @param ... ...
#' @family plot
#' @autoglobal
#' @export
plot_relative_recovery <- function(relative_data, alpha = 0.8,
                                   quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  relative_data |>
    plot_recovery(
      data = copy(relative_data)[, value := rel_value],
      alpha = alpha, quantiles = quantiles, ...
    ) +
    geom_vline(xintercept = 1, linetype = 2, size = 1.05, alpha = 0.8) +
    labs(x = "Relative value")
}

#' Plot cases by observation window
#'
#' @param cases ...
#' @family plot
#' @autoglobal
#' @export
plot_cases_by_obs_window <- function(cases) {
  cases[case_type == "primary"] |>
    ggplot() +
    aes(x = time, y = cases) +
    geom_col(aes(fill = factor(obs_at)), alpha = 1, col = "#696767b1") +
    geom_point(
      data = cases[case_type == "secondary"],
      aes(col = factor(obs_at))
    ) +
    geom_vline(
      aes(xintercept = as.numeric(as.character(obs_at))),
      linetype = 2, alpha = 0.9
    ) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    scale_color_brewer(palette = "Reds", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Cases") +
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE),
           color = guide_legend(title = "Observation day", reverse = TRUE))
}

#' Plot the empirical delay distribution
#'
#' @param cases ...
#' @param meanlog ...
#' @param sdlog ...
#' @family plot
#' @autoglobal
#' @export
plot_empirical_delay <- function(cases, meanlog, sdlog) {
  plot <- cases |>
    ggplot() +
    aes(x = delay_daily) +
    geom_histogram(
      aes(y = after_stat(density), fill = obs_at),
      binwidth = 1, position = "dodge", col = "#696767b1"
    )

  if (!missing(meanlog) && !missing(sdlog)) {
    plot <- plot +
      stat_function(
        fun = dlnorm, args = c(meanlog, sdlog), n = 100,
        col = "#696767b1"
      )
  }

  plot <- plot +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Density") +
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE))

  return(plot)
}

#' Plot the mean difference between continuous and discrete event time
#'
#' @param censor_delay ...
#' @family plot
#' @autoglobal
#' @export
plot_censor_delay <- function(censor_delay) {
  ggplot(censor_delay) +
    geom_point(aes(cohort, mean))  +
    geom_errorbar(aes(cohort, ymin = lwr, ymax = upr), width = 0) +
    facet_wrap(~type) +
    theme_bw() +
    labs(
      x = "Cohort time (day)",
      y = "Mean different between continuous and discrete event time"
    )
}

#' plot empirical cohort-based or cumulative mean vs posterior mean
#'
#' @param summarised_mean Summarised mean as produced by [summarise_variable()]
#' @param obs_mean ...
#' @param alpha ...
#' @param mean Should the mean be plotted? Logical, defaults to `FALSE`.
#' @param ribbon Should the quantile ribbon be plotted? Logical, defaults to
#' `TRUE`.
#' @param ribbon_bounds Bounds of the quantile ribbon. Defaults to
#' `c(0.05, 0.95)` which corresponds to the 90% credible interval.
#' @param ... Additional arguments passed to [ggplot2::aes()].
#'
#' @family plot
#' @autoglobal
#' @export
plot_mean_posterior_pred <- function(summarised_mean, obs_mean,
                                     alpha = 0.3, mean = FALSE, ribbon = TRUE,
                                     ribbon_bounds = c(0.05, 0.95), ...) {
  gplot <- summarised_mean |>
    ggplot()

  if (!missing(obs_mean)) {
    gplot <- gplot +
      geom_point(
        data = obs_mean,
        aes(x = ptime_daily, y = mean, size = n),
        col = "black", shape = 1
      ) +
      guides(size = guide_legend(title = "Number of samples"))
  }

  gplot <- gplot +
    labs(x = "Days prior to observation", y = "Mean delay (days)") +
    theme_bw() +
    theme(legend.position = "bottom")

  if (ribbon) {
    gplot <- gplot +
      geom_ribbon(
        aes(
          x = obs_horizon,
          ymin = .data[[make_ribbon_bound(ribbon_bounds[1])]],
          ymax = .data[[make_ribbon_bound(ribbon_bounds[2])]],
          ...
        ),
        alpha = alpha, col = NA
      )
  }
  if (mean) {
    gplot <- gplot +
      geom_line(aes(x = obs_horizon, y = mean, ...))
  }
  return(gplot)
}

make_ribbon_bound <- function(quantile) {
  paste0("q", 100 * quantile)
}

#' Plot empirical cohort-based or cumulative mean
#'
#' @param data ...
#' @family plot
#' @export
plot_cohort_mean <- function(data) {
  gplot <- ggplot(data) +
    geom_point(aes(x = ptime_daily, y = mean, size = n), shape = 1) +
    theme_bw() +
    scale_size("Number of samples") +
    labs(
      x = "Cohort time (day)",
      y = "Mean delay (days)"
    )
  return(gplot)
}
