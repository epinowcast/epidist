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

  # Only need either ptime_upr or pwindow
  # Only need either stime_upr or swindow
  
  if (!is.null(ptime_lwr)) {
    linelist <- dplyr::rename(linelist, ptime_lwr = !!ptime_lwr)
  }
  
  if (!is.null(ptime_upr)) {
    linelist <- dplyr::rename(linelist, ptime_upr = !!ptime_upr)
  }
  
  if (!is.null(pwindow)) {
    linelist <- dplyr::rename(linelist, pwindow = !!pwindow)
  }
  
  if (!is.null(stime_lwr)) {
    linelist <- dplyr::rename(linelist, stime_lwr = !!stime_lwr)
  }
  
  if (!is.null(stime_upr)) {
    linelist <- dplyr::rename(linelist, stime_upr = !!stime_upr)
  }
  
  if (!is.null(swindow)) {
    linelist <- dplyr::rename(linelist, swindow = !!swindow)
  }
  
  assert_numeric(linelist$ptime_lwr)
  assert_numeric(linelist$ptime_upr)
  assert_numeric(linelist$pwindow, lower = 0)

  assert_numeric(linelist$stime_lwr)
  assert_numeric(linelist$stime_upr)
  assert_numeric(linelist$swindow, lower = 0)
}