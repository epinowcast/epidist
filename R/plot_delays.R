#' Plot the observed delay distribution of a linelist
#'
#' @description
#' Bins the delay between the lower bounds of the primary and secondary event
#' windows of each case, the delay as it was observed, and draws the
#' proportion of cases in each bin as a column. Pass a named list of datasets
#' to compare their observed delays, and a reference distribution to draw the
#' delay distribution they are a sample of. This is the plot the vignettes use
#' to show how censoring and truncation bias the observed delays.
#'
#' @details
#' The observed delay is `stime_lwr - ptime_lwr`, the delay between the days
#' the events were reported on when the data is censored daily. It is the
#' response the naive model fits, so the plot shows the data that model sees
#' rather than the delay distribution itself. Censoring and truncation both
#' bias it, which is what the reference distribution makes visible.
#'
#' Counts are weighted by the `n` column when `data` has one, so aggregate
#' data gives the same plot as the linelist it was aggregated from.
#'
#' Proportions are within each stratum, so datasets of different sizes can be
#' compared. The density is the proportion divided by `binwidth`, which puts
#' the columns on the scale of the reference distribution.
#'
#' The plot is drawn with [ggplot2::theme_minimal()] and the colour blind
#' friendly palette the package documentation uses. Add a theme or a scale of
#' your own to the returned plot to override either. The column named by `by`
#' is kept in the plot data, so the plot can be faceted by it.
#'
#' @param data An `epidist_linelist_data` or `epidist_aggregate_data` object,
#'  or a named list of them to compare. The names label the strata.
#'
#' @param by A string naming a column of `data` to stratify the delays by.
#'  Cannot be used when `data` is a list, which is stratified by its names.
#'
#' @param binwidth The width of the delay bins, on the scale of the event
#'  times. Defaults to 1, the daily censoring the data usually has.
#'
#' @param delay_min The minimum delay to mark with a dashed vertical line, as
#'  a number on the scale of the event times. If `NULL`, the default, the
#'  `delay_min` column of `data` is used when it has one and no line is drawn
#'  otherwise.
#'
#' @param reference A named numeric vector of the distributional parameters of
#'  a delay distribution to draw over the columns, as
#'  `c(mu = 1.8, sigma = 0.5)` for a lognormal delay. If `NULL`, the default,
#'  no distribution is drawn.
#'
#' @param family A `brms` family, or the name of one, giving the delay
#'  distribution `reference` holds the parameters of. Defaults to
#'  `"lognormal"`.
#'
#' @family plot
#' @returns A `ggplot` object.
#'
#' @seealso [plot_events()] to plot the event windows the delays come from,
#'  and [plot.epidist_delay_draws()] to plot a fitted delay distribution.
#'
#' @autoglobal
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' linelist <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   )
#' plot_delays(linelist)
#' plot_delays(linelist, reference = c(mu = 1.8, sigma = 1))
#' plot_delays(list(All = linelist, Early = head(linelist, 500)))
plot_delays <- function(
  data,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal"
) {
  .check_ggplot2()
  assert_string(by, null.ok = TRUE)
  assert_number(binwidth, lower = .Machine$double.eps)
  assert_number(delay_min, null.ok = TRUE)
  strata <- .delay_strata(data, by)
  plot_data <- .bin_delays(strata$data, binwidth)
  if (!is.null(by)) {
    plot_data[[by]] <- plot_data$.stratum
  }
  if (is.null(delay_min)) {
    delay_min <- .delay_min_of(strata$datasets)
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$delay, y = .data$density)
  )
  if (is.null(strata$legend) && nlevels(plot_data$.stratum) == 1) {
    p <- p +
      ggplot2::geom_col(fill = .epidist_palette()[1], width = binwidth)
  } else {
    p <- p +
      ggplot2::geom_col(
        ggplot2::aes(fill = .data$.stratum, group = .data$.stratum),
        position = ggplot2::position_dodge2(preserve = "single"),
        width = binwidth
      ) +
      .epidist_colour_scale(nlevels(plot_data$.stratum)) +
      ggplot2::labs(fill = strata$legend)
  }
  if (!is.null(reference)) {
    p <- p + .delay_reference_layer(reference, family, plot_data, binwidth)
  }
  if (!is.null(delay_min)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = delay_min,
        linetype = "dashed",
        colour = "grey30"
      )
  }
  p <- p +
    ggplot2::labs(x = "Delay", y = "Density") +
    .epidist_plot_theme()
  return(p)
}

#' Take the observed delays of one or more datasets
#'
#' @inheritParams plot_delays
#'
#' @returns A list with `data`, a `tibble` of the observed `delay` of each
#'  case, its weight `n` and its `.stratum`, `datasets`, the datasets the
#'  delays came from, and `legend`, a legend title or `NULL`.
#'
#' @keywords internal
.delay_strata <- function(data, by = NULL) {
  if (is_epidist_linelist_data(data)) {
    datasets <- list(data)
    names(datasets) <- ""
  } else if (is.list(data) && !is.data.frame(data)) {
    datasets <- data
    if (
      length(datasets) == 0 ||
        !all(vapply(datasets, is_epidist_linelist_data, logical(1)))
    ) {
      cli_abort(
        "Every element of {.arg data} must be an
         {.cls epidist_linelist_data} object."
      )
    }
    if (is.null(names(datasets)) || !all(nzchar(names(datasets)))) {
      cli_abort("{.arg data} must be named when it is a list of datasets.")
    }
    if (!is.null(by)) {
      cli_abort(
        "{.arg by} cannot be used when {.arg data} is a list of datasets,
         which is stratified by its names."
      )
    }
  } else {
    cli_abort(
      "{.arg data} must be an {.cls epidist_linelist_data} object, or a named
       list of them."
    )
  }
  delays <- purrr::imap(datasets, function(dataset, name) {
    if (!is.null(by)) {
      assert_names(names(dataset), must.include = by)
    }
    stratum <- name
    if (!is.null(by)) {
      stratum <- as.character(dataset[[by]])
    }
    weight <- dataset[["n"]]
    if (is.null(weight)) {
      weight <- 1
    }
    return(tibble::tibble(
      .stratum = stratum,
      delay = dataset$stime_lwr - dataset$ptime_lwr,
      n = as.double(weight)
    ))
  }) |>
    purrr::list_rbind()
  delays$.stratum <- factor(delays$.stratum)
  legend_title <- by
  if (length(datasets) == 1 && is.null(by)) {
    legend_title <- NULL
  }
  return(list(data = delays, datasets = datasets, legend = legend_title))
}

#' Bin the observed delays of each stratum
#'
#' @inheritParams plot_delays
#'
#' @param delays A `tibble` of delays as returned by `.delay_strata()`.
#'
#' @returns A `tibble` with one row per stratum and bin, holding the lower
#'  edge of the bin as `delay`, the weighted count `n`, the proportion of the
#'  stratum `p` and the density `density`.
#'
#' @autoglobal
#' @keywords internal
.bin_delays <- function(delays, binwidth = 1) {
  delays$delay <- floor(delays$delay / binwidth) * binwidth
  binned <- delays |>
    dplyr::group_by(.data$.stratum, .data$delay) |>
    dplyr::summarise(n = sum(.data$n), .groups = "drop_last") |>
    dplyr::mutate(
      p = .data$n / sum(.data$n),
      density = .data$p / binwidth
    ) |>
    dplyr::ungroup()
  return(dplyr::arrange(binned, .data$.stratum, .data$delay))
}

#' Find the minimum delay the datasets record
#'
#' @param datasets A list of `epidist_linelist_data` objects.
#'
#' @returns The single minimum delay the datasets record, or `NULL` when they
#'  do not all record the same one.
#'
#' @keywords internal
.delay_min_of <- function(datasets) {
  mins <- unique(unlist(purrr::map(datasets, "delay_min")))
  if (length(mins) != 1) {
    return(NULL)
  }
  return(as.double(mins))
}

#' Draw a reference delay distribution over the observed delays
#'
#' @inheritParams plot_delays
#'
#' @param plot_data The binned delays, as returned by `.bin_delays()`.
#'
#' @returns A list of `ggplot2` layers.
#'
#' @autoglobal
#' @keywords internal
.delay_reference_layer <- function(reference, family, plot_data, binwidth) {
  assert_numeric(reference, any.missing = FALSE, names = "unique")
  delay_family <- .delay_family(.validate_family(family))
  .assert_dpars(as.list(reference), delay_family$name, delay_family$dpars)
  dens <- .delay_density_grid(
    delay_family,
    as.list(reference)[delay_family$dpars],
    max_delay = max(plot_data$delay) + binwidth
  )
  dens_line <- tibble::tibble(
    delay = dens$delays,
    density = as.vector(dens$density)
  )
  return(ggplot2::geom_line(
    data = dens_line,
    ggplot2::aes(x = .data$delay, y = .data$density),
    colour = "grey20",
    linewidth = 0.8,
    inherit.aes = FALSE
  ))
}
