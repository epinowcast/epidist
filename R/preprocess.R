as_epidist_linelist <- function(
  data, pdate_lwr = NULL, pdate_upr = NULL, sdate_lwr = NULL, sdate_upr = NULL,
  obs_date = NULL
) {
  class(data) <- c("epidist_linelist", class(data))
  
  # Rename columns to our internal names: inefficient and needs a refactor
  data <- .rename_column(data, "pdate_lwr", pdate_lwr)
  data <- .rename_column(data, "pdate_upr", pdate_upr)
  data <- .rename_column(data, "sdate_lwr", sdate_lwr)
  data <- .rename_column(data, "sdate_upr", sdate_upr)
  data <- .rename_column(data, "obs_date", obs_date)
  
  # Check for being a datetime
  checkmate::check_class(data$pdate_lwr, c("POSIXct", "POSIXlt"))
  checkmate::check_class(data$pdate_upr, c("POSIXct", "POSIXlt"))
  checkmate::check_class(data$sdate_lwr, c("POSIXct", "POSIXlt"))
  checkmate::check_class(data$sdate_upr, c("POSIXct", "POSIXlt"))
  checkmate::check_class(data$obs_date, c("POSIXct", "POSIXlt"))
  
  # Could check that all lwr < upr
  
  # Convert datetime to time
  min_date <- min(data$pdate_lwr)
  
  data <- mutate(data,
    ptime_lwr = as.numeric(pdate_lwr - min_date),
    ptime_upr = as.numeric(pdate_upr - min_date),
    stime_lwr = as.numeric(sdate_lwr - min_date),
    stime_upr = as.numeric(sdate_upr - min_date),
    obs_time = as.numeric(obs_date - min_date)
  )
  
  return(data)
}
