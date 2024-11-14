#' Observation process for primary and secondary events
#'
#' This function adds columns to linelist data representing an observation
#' process with daily primary and secondary censoring, as well as right
#' truncation. The columns added are:
#' * `ptime_daily`: The floor of `ptime`
#' * `ptime_lwr`: The lower bound of `ptime`. Equal to `ptime_daily`
#' * `ptime_upr`: The upper bound of `ptime`. Equal to `ptime_lwr + 1`
#' * `stime_daily`: The floor of `stime`
#' * `stime_lwr`: The lower bound of `stime`. Equal to `stime_daily`
#' * `stime_upr`: The upper bound of `stime`. Equal to `stime_lwr + 1`
#' * `delay_daily`: Given by `stime_daily - ptime_daily`
#' * `delay_lwr`: Given by `delay_daily - 1` (or 0 if `delay_daily < 1`)
#' * `delay_upr`: Given by `delay_daily + 1`
#' * `obs_time`: The maximum value of `stime`
#'
#' @param linelist ...
#' @family observe
#' @autoglobal
#' @export
observe_process <- function(linelist) {
  linelist |>
    mutate(
      ptime_daily = floor(.data$ptime),
      ptime_lwr = .data$ptime_daily,
      ptime_upr = .data$ptime_daily + 1,
      stime_daily = floor(.data$stime),
      stime_lwr = .data$stime_daily,
      stime_upr = .data$stime_daily + 1,
      delay_daily = .data$stime_daily - .data$ptime_daily,
      delay_lwr = purrr::map_dbl(.data$delay_daily, ~ max(0, . - 1)),
      delay_upr = .data$delay_daily + 1,
      obs_time = ceiling(max(.data$stime))
    )
}
