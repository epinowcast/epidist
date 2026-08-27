#' Extract samples of the delay distribution parameters
#'
#' @description
#' **Deprecated**
#'
#' Deprecated in favour of [delay_parameter_draws()], which returns draws in
#' the long format used by `tidybayes`, and [add_summaries()], which adds the
#' natural scale summaries this function added for you.
#'
#' @param fit A model fit with [epidist::epidist()].
#'
#' @inheritParams brms::prepare_predictions
#'
#' @param ... Additional arguments passed to [brms::prepare_predictions()].
#'
#' @family postprocess
#' @returns A `data.frame` of posterior draws of the delay distribution
#'  parameters.
#'
#' @export
predict_delay_parameters <- function(fit, newdata = NULL, ...) {
  .deprecate_warn("predict_delay_parameters", "delay_parameter_draws")
  draws <- .dpar_draws(fit, newdata = newdata, ...)
  dpars <- setdiff(names(draws), c(".row", ".chain", ".iteration", ".draw"))
  samples_df <- data.frame(draw = draws$.draw, index = draws$.row)
  for (dpar in dpars) {
    samples_df[[dpar]] <- draws[[dpar]]
  }
  delay_family <- .delay_family(fit$family)
  class(samples_df) <- c(
    paste0(delay_family$name, "_samples"),
    class(samples_df)
  )
  return(.add_mean_sd(samples_df))
}

#' @rdname predict_delay_parameters
#' @export
predict_dpar <- predict_delay_parameters

#' Add natural scale mean and standard deviation parameters
#'
#' @description
#' **Deprecated**
#'
#' Deprecated in favour of [add_summaries()], which adds the same columns for
#' the families with an analytic solution, works for every other family by
#' simulation, and adds quantiles.
#'
#' @param data A dataframe of distributional parameters.
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @returns The input with natural scale `mean` and `sd` columns added.
#'
#' @export
add_mean_sd <- function(data, ...) {
  .deprecate_warn("add_mean_sd", "add_summaries")
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
#' @returns The input with natural scale `mean` and `sd` columns added.
#'
#' @export
add_mean_sd.default <- function(data, ...) {
  .inform_no_mean_sd()
  return(data)
}

#' Add natural scale mean and standard deviation parameters for a
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
#' @returns The input with natural scale `mean` and `sd` columns added.
#'
#' @export
add_mean_sd.lognormal_samples <- function(data, ...) {
  return(.analytic_summaries(data, .analytic_delay_summaries("lognormal")))
}

#' Add natural scale mean and standard deviation parameters for a Gamma
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
#' @returns The input with natural scale `mean` and `sd` columns added.
#'
#' @export
add_mean_sd.gamma_samples <- function(data, ...) {
  return(.analytic_summaries(data, .analytic_delay_summaries("gamma")))
}

#' Add natural scale mean and standard deviation parameters for a Weibull
#' model
#'
#' Note that the input parameters here are `mu` and `shape`, corresponding to
#' the distributional parameters used by `brms` for the `weibull` family.
#'
#' @inheritParams add_mean_sd
#'
#' @param ... Additional arguments for method.
#'
#' @family postprocess
#' @method add_mean_sd weibull_samples
#' @returns The input with natural scale `mean` and `sd` columns added.
#'
#' @export
add_mean_sd.weibull_samples <- function(data, ...) {
  return(.analytic_summaries(data, .analytic_delay_summaries("weibull")))
}

#' Add analytic mean and standard deviation columns without deprecating
#'
#' Used by [predict_delay_parameters()], which added these columns before
#' [add_summaries()] existed.
#'
#' @inheritParams add_mean_sd
#'
#' @return The input with natural scale `mean` and `sd` columns added, or
#'  unchanged when the family has no analytic solution.
#'
#' @keywords internal
.add_mean_sd <- function(data) {
  delay_family <- tryCatch(
    .resolve_delay_family(data),
    error = function(e) NULL
  )
  analytic <- NULL
  if (!is.null(delay_family)) {
    analytic <- .analytic_delay_summaries(delay_family$name)
  }
  if (is.null(analytic) || !all(analytic$dpars %in% names(data))) {
    .inform_no_mean_sd()
    return(data)
  }
  return(.analytic_summaries(data, analytic))
}

#' Tell the user that no analytic mean and standard deviation are available
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
.inform_no_mean_sd <- function() {
  cli_inform(c(
    "!" = "Natural scale mean and standard deviation parameter columns not
    added: no method available for this family",
    "Consider submitting an issue to https:/github.com/epinowcast/epidist"
  ))
  return(invisible(NULL))
}

#' Warn that a function is deprecated
#'
#' @param what The name of the deprecated function.
#'
#' @param with The name of the function to use instead.
#'
#' @param when The version the function was deprecated in.
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
.deprecate_warn <- function(what, with, when = "0.5.0") {
  cli_warn(
    c(
      "!" = "{.fn {what}} was deprecated in {.pkg epidist} {when}.",
      i = "Use {.fn {with}} instead."
    ),
    class = "epidist_deprecated_warning"
  )
  return(invisible(NULL))
}
