#' Plot the observed delay distribution
#'
#' @description
#' Bins the delay between the lower bounds of the primary and secondary event
#' windows of each case, the delay as it was observed, and draws the
#' proportion of cases in each bin as a column. Pass a named list of datasets
#' to compare their observed delays, a reference distribution to draw the
#' delay distribution they are a sample of, or a model fitted with [epidist()]
#' to draw the delays it predicts over the delays it was fitted to.
#'
#' @details
#' The observed delay is `stime_lwr - ptime_lwr`, the delay between the days
#' the events were reported on when the data is censored daily. It is the
#' response the naive model fits, so the plot shows the data that model sees
#' rather than the delay distribution itself. Censoring and truncation both
#' bias it, which is what the reference distribution makes visible.
#'
#' Counts are weighted by the `n` column when the data has one, so aggregate
#' data gives the same plot as the linelist it was aggregated from.
#'
#' Proportions are within each stratum, so datasets of different sizes can be
#' compared. The density is the proportion divided by `binwidth`, which puts
#' the columns on the scale of the reference distribution.
#'
#' The columns named by `by` are kept in the plot data, so the plot can be
#' faceted by them.
#'
#' @param x An `epidist_linelist_data` or `epidist_aggregate_data` object, a
#'  named list of them to compare, or a model fitted with [epidist()].
#'
#' @param ... Passed to the method.
#'
#' @family plot
#' @returns A `ggplot` object.
#'
#' @seealso [plot_events()] to plot the event windows the delays come from,
#'  and [plot.epidist_delay_draws()] to plot a fitted delay distribution.
#'
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
plot_delays <- function(x, ...) {
  UseMethod("plot_delays")
}

#' Default method for plotting observed delays
#'
#' @inheritParams plot_delays
#'
#' @method plot_delays default
#' @family plot
#' @returns This method errors. It is called when `x` is neither delay data
#'  nor a fitted model.
#'
#' @export
plot_delays.default <- function(x, ...) {
  return(cli_abort(
    "{.arg x} must be an {.cls epidist_linelist_data} object, a named list of
     them, or a model fitted with {.fn epidist}."
  ))
}

#' Plot the observed delays of a dataset
#'
#' @inheritParams plot_delays
#'
#' @param by A string naming a column of `x` to stratify the delays by.
#'
#' @param binwidth The width of the delay bins, on the scale of the event
#'  times. Defaults to 1, the daily censoring the data usually has.
#'
#' @param delay_min The minimum delay to mark with a dashed vertical line, as
#'  a number on the scale of the event times. If `NULL`, the default, the
#'  `delay_min` column of `x` is used when it has one and no line is drawn
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
#' @method plot_delays epidist_linelist_data
#' @family plot
#' @returns A `ggplot` object.
#'
#' @export
plot_delays.epidist_linelist_data <- function(
  x,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal",
  ...
) {
  return(.plot_observed_delays(
    list(all = x),
    by = by,
    binwidth = binwidth,
    delay_min = delay_min,
    reference = reference,
    family = family
  ))
}

#' Compare the observed delays of several datasets
#'
#' The names of `x` label the strata.
#'
#' @inheritParams plot_delays.epidist_linelist_data
#'
#' @method plot_delays list
#' @family plot
#' @returns A `ggplot` object.
#'
#' @export
plot_delays.list <- function(
  x,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal",
  ...
) {
  .check_ggplot2()
  if (
    length(x) == 0 || !all(vapply(x, is_epidist_linelist_data, logical(1)))
  ) {
    cli_abort(
      "Every element of {.arg x} must be an
       {.cls epidist_linelist_data} object."
    )
  }
  if (is.null(names(x)) || !all(nzchar(names(x)))) {
    cli_abort("{.arg x} must be named when it is a list of datasets.")
  }
  if (!is.null(by)) {
    cli_abort(
      "{.arg by} cannot be used when {.arg x} is a list of datasets, which is
       stratified by its names."
    )
  }
  return(.plot_observed_delays(
    x,
    by = NULL,
    binwidth = binwidth,
    delay_min = delay_min,
    reference = reference,
    family = family
  ))
}

#' Plot the delays a fitted model predicts over the delays it was fitted to
#'
#' @description
#' Draws the observed delays of the data the model was fitted to as columns,
#' as [plot_delays.epidist_linelist_data()] does, and the delays the model
#' predicts for the same cases over them, as the posterior median proportion
#' in each bin with a ribbon between two quantiles.
#'
#' @details
#' The predictions come from [brms::posterior_predict()], so they are of the
#' delay as it was observed, under the censoring and truncation of each case.
#' That makes the plot a posterior predictive check of the observed delays
#' rather than a picture of the delay distribution itself, which
#' [plot.epidist_delay_draws()] draws.
#'
#' Both the columns and the predictions are proportions within each stratum,
#' so a bin holds the share of cases with that delay. Bins where neither the
#' data nor the predictive interval puts any mass are dropped, which trims the
#' tail of the predictive distribution.
#'
#' @inheritParams plot_delays
#' @inheritParams plot_delays.epidist_linelist_data
#'
#' @param by A character vector of columns of the model data that define the
#'  strata to colour by. If `NULL`, the default, the variables in the
#'  distributional parameter formulas are used, as [epidist_strata()] does.
#'
#' @param ndraws The number of posterior draws to predict from, sampled at
#'  random. Defaults to 100, which is enough for the median and the
#'  quantiles of a binned distribution and bounds the size of the
#'  prediction. Use `NULL` to predict from every draw.
#'
#' @param probs A numeric vector of two probabilities giving the quantiles the
#'  ribbon spans. Defaults to `c(0.05, 0.95)`.
#'
#' @method plot_delays epidist_fit
#' @family plot
#' @returns A `ggplot` object.
#'
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' \donttest{
#' fit <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data() |>
#'   as_epidist_marginal_model() |>
#'   epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#'
#' plot_delays(fit)
#' }
plot_delays.epidist_fit <- function(
  x,
  by = NULL,
  binwidth = 1,
  ndraws = 100,
  probs = c(0.05, 0.95),
  ...
) {
  .check_ggplot2()
  assert_character(by, any.missing = FALSE, null.ok = TRUE)
  assert_number(binwidth, lower = .Machine$double.eps)
  assert_count(ndraws, positive = TRUE, null.ok = TRUE)
  assert_numeric(
    probs,
    lower = 0,
    upper = 1,
    len = 2,
    any.missing = FALSE,
    sorted = TRUE
  )
  observed <- .fitted_delays(x, by)
  strata <- .draw_strata(observed$data, observed$by)
  plot_data <- .bin_delays(strata$data, binwidth, keep = strata$by)
  predicted <- .predicted_delays(x, strata$data, binwidth, ndraws, probs)
  predicted <- dplyr::semi_join(
    predicted,
    .delay_bins_with_mass(plot_data, predicted),
    by = c(".stratum", "delay")
  )

  p <- .delay_column_plot(plot_data, strata$legend, binwidth) +
    .delay_predictive_layers(predicted, is.null(strata$by))
  return(.delay_plot_labs(p))
}

#' Take the observed delays of a fitted model
#'
#' Reads the response and the case weights of the model data, and the strata
#' to colour by, which default to the variables in the distributional
#' parameter formulas as they do in [epidist_strata()].
#'
#' @inheritParams plot_delays.epidist_fit
#'
#' @returns A list with `data`, a `tibble` of the observed `delay` of each
#'  case, its weight `n` and the columns in `by`, and `by`, the columns the
#'  strata are defined by or `NULL` when there are none.
#'
#' @keywords internal
.fitted_delays <- function(x, by = NULL) {
  model_data <- x$data
  if (is.null(model_data)) {
    cli_abort("{.arg x} does not contain the data it was fitted to.")
  }
  family_name <- x$family$name
  if (is.null(family_name)) {
    family_name <- x$family$family
  }
  if (startsWith(tolower(family_name), "meta_")) {
    cli_abort(c(
      "{.fn plot_delays} cannot plot a meta analytic model.",
      i = "It is fitted to summary estimates rather than to observed delays."
    ))
  }
  model_data <- tibble::as_tibble(model_data)
  if (is.null(by)) {
    by <- intersect(.extract_dpar_terms(x$formula), names(model_data))
    if (length(by) == 0) {
      by <- NULL
    }
  } else {
    assert_names(names(model_data), must.include = by)
  }
  response <- all.vars(brms::brmsterms(x$formula)$respform)[1]
  weight <- model_data[["n"]]
  if (is.null(weight)) {
    weight <- 1
  }
  delays <- tibble::tibble(
    delay = as.double(model_data[[response]]),
    n = as.double(weight)
  )
  if (!is.null(by)) {
    delays <- dplyr::bind_cols(delays, model_data[by])
  }
  return(list(data = delays, by = by))
}

#' Bin the delays a fitted model predicts for each case
#'
#' Predicts the delay of every case in the model data from each posterior
#' draw, bins the predictions of each draw as [.bin_delays()] bins the
#' observed delays, and summarises the proportion in each bin across the
#' draws. Draws with no prediction in a bin count as a proportion of zero.
#'
#' @inheritParams plot_delays.epidist_fit
#'
#' @param delays A `tibble` of the observed delays with their `.stratum`, as
#'  returned by [.draw_strata()].
#'
#' @returns A `tibble` with one row per stratum and bin, holding the lower
#'  edge of the bin as `delay`, the posterior median `density` and the
#'  `lower` and `upper` quantiles of it.
#'
#' @autoglobal
#' @keywords internal
.predicted_delays <- function(
  x,
  delays,
  binwidth = 1,
  ndraws = 100,
  probs = c(0.05, 0.95)
) {
  predictions <- brms::posterior_predict(x, ndraws = ndraws)
  n_draws <- nrow(predictions)
  binned <- tibble::tibble(
    .draw = rep(seq_len(n_draws), times = ncol(predictions)),
    .stratum = rep(delays$.stratum, each = n_draws),
    delay = .floor_mult(as.vector(predictions), binwidth),
    n = rep(delays$n, each = n_draws)
  ) |>
    dplyr::group_by(.data$.draw, .data$.stratum, .data$delay) |>
    dplyr::summarise(n = sum(.data$n), .groups = "drop_last") |>
    dplyr::mutate(p = .data$n / sum(.data$n)) |>
    dplyr::ungroup()
  summarised <- binned |>
    dplyr::group_by(.data$.stratum, .data$delay) |>
    dplyr::summarise(
      density = .zero_padded_quantile(.data$p, n_draws, 0.5) / binwidth,
      lower = .zero_padded_quantile(.data$p, n_draws, probs[1]) / binwidth,
      upper = .zero_padded_quantile(.data$p, n_draws, probs[2]) / binwidth,
      .groups = "drop"
    )
  return(summarised)
}

#' Take a quantile of the proportions of every draw
#'
#' A draw with no prediction in a bin has no row for it, and so a proportion
#' of zero. Pads the proportions back up to one per draw before taking the
#' quantile.
#'
#' @param p The proportions of the draws that predicted into the bin.
#'
#' @param n_draws The number of draws.
#'
#' @param prob The probability of the quantile to take.
#'
#' @returns A number.
#'
#' @keywords internal
.zero_padded_quantile <- function(p, n_draws, prob) {
  padded <- c(p, rep(0, n_draws - length(p)))
  return(unname(stats::quantile(padded, probs = prob)))
}

#' Find the bins the data or the predictions put mass in
#'
#' @param observed The binned observed delays, as returned by
#'  [.bin_delays()].
#'
#' @param predicted The binned predicted delays, as returned by
#'  [.predicted_delays()].
#'
#' @returns A `tibble` of the `.stratum` and `delay` of each bin to draw.
#'
#' @keywords internal
.delay_bins_with_mass <- function(observed, predicted) {
  return(dplyr::bind_rows(
    observed[observed$n > 0, c(".stratum", "delay")],
    predicted[predicted$upper > 0, c(".stratum", "delay")]
  ))
}

#' Plot the observed delays of one or more datasets
#'
#' @inheritParams plot_delays.epidist_linelist_data
#'
#' @param datasets A named list of `epidist_linelist_data` objects.
#'
#' @returns A `ggplot` object.
#'
#' @keywords internal
.plot_observed_delays <- function(
  datasets,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal"
) {
  .check_ggplot2()
  assert_string(by, null.ok = TRUE)
  assert_number(binwidth, lower = .Machine$double.eps)
  assert_number(delay_min, lower = 0, null.ok = TRUE)
  strata <- .delay_strata(datasets, by)
  plot_data <- .bin_delays(strata$data, binwidth)
  if (!is.null(by)) {
    plot_data[[by]] <- plot_data$.stratum
  }
  if (is.null(delay_min)) {
    delay_min <- .delay_min_of(datasets)
  }

  p <- .delay_column_plot(plot_data, strata$legend, binwidth)
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
  return(.delay_plot_labs(p))
}

#' Label the strata of the observed delays of one or more datasets
#'
#' Takes the observed delay of each case, labels it with the dataset it came
#' from, and hands the labelling to [.draw_strata()]. A single dataset with no
#' `by` has one stratum, and a list of datasets is stratified by its names.
#'
#' @inheritParams .plot_observed_delays
#'
#' @returns A list as returned by [.draw_strata()], holding the observed
#'  `delay` of each case, its weight `n` and its `.stratum`.
#'
#' @keywords internal
.delay_strata <- function(datasets, by = NULL) {
  delays <- purrr::imap(datasets, function(dataset, name) {
    if (!is.null(by)) {
      assert_names(names(dataset), must.include = by)
    }
    weight <- dataset[["n"]]
    if (is.null(weight)) {
      weight <- 1
    }
    out <- tibble::tibble(
      .dataset = name,
      delay = dataset$stime_lwr - dataset$ptime_lwr,
      n = as.double(weight)
    )
    if (!is.null(by)) {
      out[[by]] <- dataset[[by]]
    }
    return(out)
  }) |>
    purrr::list_rbind()
  stratify_by <- by
  if (is.null(by) && length(datasets) > 1) {
    stratify_by <- ".dataset"
  }
  strata <- .draw_strata(delays, stratify_by)
  if (identical(stratify_by, ".dataset")) {
    # The names of the datasets are the labels, so they need no legend title
    strata$legend <- NULL
  }
  return(strata)
}

#' Bin the delays of each stratum
#'
#' @inheritParams plot_delays.epidist_linelist_data
#'
#' @param delays A `tibble` of delays with their `.stratum`, as returned by
#'  [.draw_strata()].
#'
#' @param keep A character vector of columns of `delays` to keep, which must
#'  be constant within a stratum.
#'
#' @returns A `tibble` with one row per stratum and bin, holding the lower
#'  edge of the bin as `delay`, the weighted count `n`, the proportion of the
#'  stratum `p` and the density `density`.
#'
#' @autoglobal
#' @keywords internal
.bin_delays <- function(delays, binwidth = 1, keep = NULL) {
  delays$delay <- .floor_mult(delays$delay, binwidth)
  binned <- delays |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(keep, ".stratum"))),
      .data$delay
    ) |>
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

#' Draw the binned delays of each stratum as columns
#'
#' @inheritParams plot_delays.epidist_linelist_data
#'
#' @param plot_data The binned delays, as returned by [.bin_delays()].
#'
#' @param legend The legend title, or `NULL` for none.
#'
#' @returns A `ggplot` object.
#'
#' @autoglobal
#' @keywords internal
.delay_column_plot <- function(plot_data, legend, binwidth) {
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$delay, y = .data$density)
  )
  if (is.null(legend) && nlevels(plot_data$.stratum) == 1) {
    return(
      p + ggplot2::geom_col(fill = .epidist_palette()[1], width = binwidth)
    )
  }
  return(
    p +
      ggplot2::geom_col(
        ggplot2::aes(fill = .data$.stratum, group = .data$.stratum),
        position = ggplot2::position_dodge2(preserve = "single"),
        width = binwidth
      ) +
      .epidist_colour_scale(nlevels(plot_data$.stratum)) +
      ggplot2::labs(fill = legend)
  )
}

#' Draw the predicted delays over the observed delays
#'
#' @inheritParams .delay_bins_with_mass
#'
#' @param single A logical, `TRUE` when there is a single stratum.
#'
#' @returns A list of `ggplot2` layers.
#'
#' @autoglobal
#' @keywords internal
.delay_predictive_layers <- function(predicted, single) {
  if (single) {
    return(list(
      ggplot2::geom_ribbon(
        data = predicted,
        ggplot2::aes(
          x = .data$delay,
          ymin = .data$lower,
          ymax = .data$upper
        ),
        fill = "grey50",
        alpha = 0.3,
        inherit.aes = FALSE
      ),
      ggplot2::geom_line(
        data = predicted,
        ggplot2::aes(x = .data$delay, y = .data$density),
        colour = "grey20",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
    ))
  }
  return(list(
    ggplot2::geom_ribbon(
      data = predicted,
      ggplot2::aes(
        x = .data$delay,
        ymin = .data$lower,
        ymax = .data$upper,
        fill = .data$.stratum
      ),
      alpha = 0.3,
      inherit.aes = FALSE
    ),
    ggplot2::geom_line(
      data = predicted,
      ggplot2::aes(
        x = .data$delay,
        y = .data$density,
        colour = .data$.stratum
      ),
      linewidth = 0.8,
      inherit.aes = FALSE
    )
  ))
}

#' Draw a reference delay distribution over the observed delays
#'
#' @inheritParams plot_delays.epidist_linelist_data
#' @inheritParams .delay_column_plot
#'
#' @returns A `ggplot2` layer.
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

#' Label the axes of a delay plot and theme it
#'
#' @param p A `ggplot` object.
#'
#' @returns A `ggplot` object.
#'
#' @keywords internal
.delay_plot_labs <- function(p) {
  return(
    p +
      ggplot2::labs(x = "Delay", y = "Density") +
      .epidist_plot_theme()
  )
}
