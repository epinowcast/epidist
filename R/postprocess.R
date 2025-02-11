#' Extract samples of the delay distribution parameters
#'
#' @param fit A model fit with [epidist::epidist()].
#'
#' @inheritParams brms::prepare_predictions
#'
#' @param ... Additional arguments passed to [brms::prepare_predictions()].
#'
#' @family postprocess
#' @autoglobal
#' @export
predict_delay_parameters <- function(fit, newdata = NULL, ...) {
  if (!is.null(newdata)) {
    newdata <- brms::validate_newdata(newdata, fit)
  }
  pp <- brms::prepare_predictions(fit, newdata = newdata, ...)
  # Every brms model has the parameter mu
  lp_mu <- brms::get_dpar(pp, dpar = "mu", inv_link = TRUE)
  df <- expand.grid(
    draw = seq_len(nrow(lp_mu)),
    index = seq_len(ncol(lp_mu))
  )
  df[["mu"]] <- as.vector(lp_mu)
  for (dpar in setdiff(names(pp$dpars), "mu")) {
    lp_dpar <- brms::get_dpar(pp, dpar = dpar, inv_link = TRUE)
    df[[dpar]] <- as.vector(lp_dpar)
  }
  class(df) <- c(
    paste0(sub(".*_", "", fit$family$name), "_samples"), class(df)
  )
  df <- add_mean_sd(df)
  return(df)
}

#' @rdname predict_delay_parameters
#' @export
predict_dpar <- predict_delay_parameters

#' Add natural scale mean and standard deviation parameters
#'
#' @param data A dataframe of distributional parameters.
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @export
add_mean_sd <- function(data, ...) {
  UseMethod("add_mean_sd")
}

#' Default method for add natural scale parameters
#'
#' @inheritParams add_mean_sd
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @method add_mean_sd default
#' @export
add_mean_sd.default <- function(data, ...) {
  cli_inform(c(
    "!" = "Natural scale mean and standard deviation parameter columns not
    added: no method available for this family",
    "Consider submitting an issue to https:/github.com/epinowcast/epidist"
  ))
  return(data)
}

#' Add natural scale mean and standard deviation parameters for a latent
#' lognormal model
#'
#' Note that the input parameters here are `mu` and `sigma`, corresponding to
#' the distributional parameters used by `brms` for the `lognormal` family.
#'
#' @inheritParams add_mean_sd
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @method add_mean_sd lognormal_samples
#' @autoglobal
#' @export
add_mean_sd.lognormal_samples <- function(data, ...) {
  mutate(data,
    mean = exp(.data$mu + .data$sigma^2 / 2),
    sd = .data$mean * sqrt(exp(.data$sigma^2) - 1)
  )
}

#' Add natural scale mean and standard deviation parameters for a latent gamma
#' model
#'
#' Again, `mu` and `shape` here are the distributional parameters of `brms`.
#'
#' @inheritParams add_mean_sd
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @method add_mean_sd gamma_samples
#' @autoglobal
#' @export
add_mean_sd.gamma_samples <- function(data, ...) {
  mutate(data,
    mean = .data$mu,
    sd = .data$mu / sqrt(.data$shape)
  )
}
