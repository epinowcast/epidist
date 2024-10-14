add_obs_vars <- function(
    linelist
) {
  return(linelist)
}

.rename_column <- function(df, new, old) {
  are_char <- is.character(new) & is.character(old)
  if (are_char) {
    df <- dplyr::rename(df, !!new := !!old)
  }
  return(df)
}