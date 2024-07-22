#' Add natural scale parameters
#'
#' @param data A dataframe of distributional parameters
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
#' @method add_natural_scale_mean_sd default
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
#' Note that the input parameters here are `mu` and `sigma`, corresponding to
#' the distributional parameters used by `brms` for the `lognormal` family.
#'
#' @inheritParams add_natural_scale_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @method add_natural_scale_mean_sd latent_lognormal
#' @autoglobal
#' @export
add_natural_scale_mean_sd.latent_lognormal <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  nat_dt <- nat_dt[, mean := exp(mu + sigma ^ 2 / 2)]
  nat_dt <- nat_dt[, sd := mu * sqrt(exp(sigma ^ 2) - 1)]
  return(nat_dt[])
}

#' Add natural scale parameters for a latent gamma model
#'
#' Again, `mu` and `shape` here are the distributional parameters of `brms`.
#'
#' @inheritParams add_natural_scale_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @method add_natural_scale_mean_sd latent_gamma
#' @autoglobal
#' @export
add_natural_scale_mean_sd.latent_gamma <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  nat_dt <- nat_dt[, mean := sigma]
  nat_dt <- nat_dt[, sd := mu / sqrt(shape)]
  return(nat_dt[])
}
