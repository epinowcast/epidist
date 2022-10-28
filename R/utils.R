#' Save a plot and return the path for targets
#' @importFrom ggplot2 ggsave
save_plot <- function(plot, filename, path, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, file)
  ggplot2::ggsave(path, plot, ...)
  return(filename)
}

#' Save a dataframe to a csv and return the path for targets
save_csv <- function(dt, filename, path, allow_empty = TRUE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, filename)

  if (allow_empty | nrow(dt) > 0) {
    data.table::fwrite(dt, path)
  }
  return(path)
}