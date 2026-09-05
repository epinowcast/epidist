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
      colour = "grey"
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
        linewidth = 1.5
      ) +
      ggplot2::scale_colour_manual(
        values = c(primary = "#56B4E9", secondary = "#009E73")
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
        linewidth = 1.5
      ) +
      ggplot2::labs(colour = by)
  }
  if (!is.null(obs_time)) {
    p <- p +
      ggplot2::geom_vline(xintercept = obs_time, linetype = "dashed")
  }
  p <- p +
    ggplot2::labs(
      x = ifelse(use_dates, "Event date", "Event time"),
      y = "Case"
    )
  return(p)
}

#' Check that `ggplot2` is installed
#'
#' `ggplot2` is a suggested package, so the plot functions check for it before
#' they use it.
#'
#' @return `NULL`, invisibly.
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
