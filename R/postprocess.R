#' Extract samples of the delay distribution parameters
#'
#' @param fit A model fit with `epidist::epidist`
#' @inheritParams brms::prepare_predictions
#' @param ... Additional arguments passed to `brms::prepare_predictions`
#' @family postprocess
#' @autoglobal
#' @export
predict_delay_parameters <- function(fit, newdata = NULL, ...) {
  if (!is.null(newdata)) {
    newdata <- brms:::validate_newdata(newdata, fit)
  }
  pp <- brms::prepare_predictions(fit, newdata = newdata, ...)
  # Every brms model has the parameter mu
  lp_mu <- brms::get_dpar(pp, dpar = "mu", inv_link = TRUE)
  df <- expand.grid(
    "draw" = seq_len(nrow(lp_mu)),
    "index" = seq_len(ncol(lp_mu))
  )
  df[["mu"]] <- as.vector(lp_mu)
  for (dpar in setdiff(names(pp$dpars), "mu")) {
    lp_dpar <- brms::get_dpar(pp, dpar = dpar, inv_link = TRUE)
    df[[dpar]] <- as.vector(lp_dpar)
  }
  class(df) <- c(
    class(df), paste0(sub(".*_", "", fit$family$name), "_samples")
  )
  dt <- as.data.table(df)
  dt <- add_mean_sd(dt)
  return(dt)
}

#' @rdname predict_delay_parameters
#' @export
predict_dpar <- predict_delay_parameters

#' Convert posterior lognormal samples to long format
#'
#' @param draws ...
#' @family postprocess
#' @export
draws_to_long <- function(draws) {
  long_draws <- data.table::melt(
    draws,
    measure.vars = c("mu", "sigma", "mean", "sd"),
    variable.name = "parameter"
  )
  return(long_draws[])
}

#' Make posterior lognormal samples relative to true values
#'
#' @param draws ...
#' @param secondary_dist ...
#' @param by ...
#' @family postprocess
#' @autoglobal
#' @export
make_relative_to_truth <- function(draws, secondary_dist, by = "parameter") {
  draws <- merge(
    draws,
    secondary_dist[, true_value := value][, value := NULL],
    by = by
  )

  draws[, rel_value := value / true_value]

  return(draws[])
}

#' Summarise posterior draws
#'
#' @param draws A data.table of posterior draws
#' @param sf The number of significant figures to use
#' @param not_by ...
#' @param by A vector of columns to group by
#' @family postprocess
#' @importFrom stats median quantile
#' @export
summarise_draws <- function(draws, sf, not_by = "value", by) {
  if (missing(by)) {
    by_cols <- setdiff(
      colnames(draws), not_by
    )
  }else {
    by_cols <- by
  }

  summarised_draws <- draws[,
    list(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      q2.5 = quantile(value, 0.025, na.rm = TRUE),
      q5 = quantile(value, 0.05, na.rm = TRUE),
      q20 = quantile(value, 0.2, na.rm = TRUE),
      q35 = quantile(value, 0.35, na.rm = TRUE),
      q65 = quantile(value, 0.65, na.rm = TRUE),
      q80 = quantile(value, 0.8, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE),
      q97.5 = quantile(value, 0.975, na.rm = TRUE)
    ),
    by = by_cols
  ]

  if (!missing(sf)) {
    cols <- setdiff(colnames(summarised_draws), by_cols)
    summarised_draws <- summarised_draws[,
      (cols) := lapply(.SD, signif, digits = sf),
      .SDcols = cols
    ]
  }

  return(summarised_draws[])
}

#' Summarise a variable
#'
#' @param variable The variable to summarise
#' @inheritParams summarise_draws
#'
#' @family postprocess
#' @autoglobal
#' @export
summarise_variable <- function(draws, variable, sf = 6, by = c()) {
  if (missing(variable)) {
    stop("variable must be specified")
  }
  summarised_draws <- data.table::copy(draws)
  summarised_draws[, value := variable, env = list(variable = variable)]
  summarised_draws <- summarise_draws(summarised_draws, sf = sf, by = by)
  return(summarised_draws[])
}

#' Add natural scale mean and standard deviation parameters
#'
#' @param data A dataframe of distributional parameters
#' @param ... Additional arguments for method.
#' @family postprocess
#' @export
add_mean_sd <- function(data, ...) {
  UseMethod("add_mean_sd")
}

#' Default method for add natural scale parameters
#'
#' @inheritParams add_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @method add_mean_sd default
#' @export
add_mean_sd.default <- function(data, ...) {
  cli::cli_inform(c(
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
#' @param ... Additional arguments for method.
#' @family postprocess
#' @method add_mean_sd lognormal_samples
#' @autoglobal
#' @export
add_mean_sd.lognormal_samples <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  nat_dt <- nat_dt[, mean := exp(mu + sigma ^ 2 / 2)]
  nat_dt <- nat_dt[, sd := mu * sqrt(exp(sigma ^ 2) - 1)]
  return(nat_dt[])
}

#' Add natural scale mean and standard deviation parameters for a latent gamma
#' model
#'
#' Again, `mu` and `shape` here are the distributional parameters of `brms`.
#'
#' @inheritParams add_mean_sd
#' @param ... Additional arguments for method.
#' @family postprocess
#' @method add_mean_sd gamma_samples
#' @autoglobal
#' @export
add_mean_sd.gamma_samples <- function(data, ...) {
  nat_dt <- data.table::copy(data)
  nat_dt <- nat_dt[, mean := mu]
  nat_dt <- nat_dt[, sd := mu / sqrt(shape)]
  return(nat_dt[])
}
