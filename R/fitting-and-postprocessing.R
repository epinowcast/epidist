#' Extract posterior samples for a lognormal brms model
#'
#' @param data ...
#' @param id_vars ...
#' @param from_dt ...
#' @family postprocess
#' @export
#' @autoglobal
#' @importFrom posterior as_draws_df
extract_lognormal_draws <- function(data, id_vars, from_dt = FALSE) {
  if (from_dt) {
    if (!any(colnames(data) %in% "fit")) {
      return(id_vars[])
    }
    draws <- data$fit[[1]]$draws(variables = c("Intercept", "Intercept_sigma"))
  }else {
    draws <- data
  }

  draws <- posterior::as_draws_df(draws) |>
    data.table::as.data.table()

  draws <- draws[, mu := Intercept]
  draws <- draws[, sigma := exp(Intercept_sigma)]
  draws <- draws[, list(mu, sigma)]
  class(draws) <- c(class(draws), "lognormal_samples")
  draws <- add_natural_scale_mean_sd(draws)

  if (!missing(id_vars)) {
    draws <- merge(
      draws[, id := id_vars$id], id_vars, by = "id"
    )
  }

  return(draws[])
}

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
