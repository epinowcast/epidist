#' Add columns for interval censoring of primary and secondary events
#'
#' @param linelist ...
#' @param ptime_lwr ...
#' @param ptime_upr ...
#' @param pwindow ...
#' @param stime_lwr ...
#' @param stime_upr ...
#' @param swindow ...
#' @family preprocess
#' @autoglobal
#' @export
add_event_vars <- function(
  linelist, ptime_lwr = NULL, ptime_upr = NULL, pwindow = NULL,
  stime_lwr = NULL, stime_upr = NULL, swindow = NULL
) {
  if (!is.null(ptime_lwr)) {
    linelist <- dplyr::rename(linelist, ptime_lwr = !!ptime_lwr)
  }

  if (!is.null(ptime_upr)) {
    linelist <- dplyr::rename(linelist, ptime_upr = !!ptime_upr)
  }

  if (!is.null(pwindow) && is.character(pwindow)) {
    linelist <- dplyr::rename(linelist, pwindow = !!pwindow)
  }

  if (!is.null(stime_lwr)) {
    linelist <- dplyr::rename(linelist, stime_lwr = !!stime_lwr)
  }

  if (!is.null(stime_upr)) {
    linelist <- dplyr::rename(linelist, stime_upr = !!stime_upr)
  }

  if (!is.null(swindow) && is.character(swindow)) {
    linelist <- dplyr::rename(linelist, swindow = !!swindow)
  }

  if (is.numeric(pwindow)) {
    # Warn that numeric pwindow overwrites pwindow in data
    linelist$pwindow <- pwindow
  }

  if (is.numeric(swindow)) {
    # Warn that numeric swindow overwrites swindow in data
    linelist$swindow <- swindow
  }

  if (is.null(stime_upr)) {
    linelist <- mutate(linelist, stime_upr = stime_lwr + swindow)
  }

  if (is.null(ptime_upr)) {
    linelist <- mutate(linelist, ptime_upr = ptime_lwr + pwindow)
  }

  if (is.null(swindow)) {
    linelist <- mutate(linelist, pwindow = stime_upr - stime_lwr)
  }

  if (is.null(pwindow)) {
    linelist <- mutate(linelist, swindow = ptime_upr - ptime_lwr)
  }

  assert_numeric(linelist$ptime_lwr)
  assert_numeric(linelist$ptime_upr)
  assert_numeric(linelist$pwindow, lower = 0)
  assert_true(
    all(linelist$ptime_lwr + linelist$pwindow - linelist$ptime_upr < 1e-6)
  )

  assert_numeric(linelist$stime_lwr)
  assert_numeric(linelist$stime_upr)
  assert_numeric(linelist$swindow, lower = 0)
  assert_true(
    all(linelist$stime_lwr + linelist$swindow - linelist$stime_upr < 1e-6)
  )

  linelist <- dplyr::relocate(
    linelist, ptime_lwr, ptime_upr, pwindow, stime_lwr, stime_upr, swindow
  )

  return(linelist)
}
