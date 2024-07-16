#' Add natural scale parameters
#'
#' @param data A dataframe to be used for modelling.
#' @param ... Additional arguments for method.
#' @family postprocess
#' @export
add_natural_scale_mean_sd <- function(data, ...) {
  UseMethod("add_natural_scale_mean_sd")
}

#' Default method for add natural scale parameters
#'
#' @inheritParams add_natural_scale_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @export
add_natural_scale_mean_sd.default <- function(data, ...) {
  cli::cli_inform(
    "!" = "Natural scale mean and standard deviation parameters columns not
           added: no method available for this family",
    "Consider submitting an issue to https:/github.com/epinowcast/epidist"
  )
  return(data)
}

#' Add natural scale parameters for a latent lognormal model
#'
#' @inheritParams add_natural_scale_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @autoglobal
#' @export
add_natural_scale_mean_sd.latent_lognormal <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  nat_dt <- nat_dt[, mean := exp(meanlog + sdlog ^ 2 / 2)]
  nat_dt <- nat_dt[, sd := mean * sqrt(exp(sdlog ^ 2) - 1)]
  return(nat_dt[])
}

#' Add natural scale parameters for a latent gamma model
#'
#' @inheritParams add_natural_scale_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @autoglobal
#' @export
add_natural_scale_mean_sd.latent_gamma <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  # Do something here!
  return(nat_dt[])
}
