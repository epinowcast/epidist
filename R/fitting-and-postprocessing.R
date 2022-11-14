#' Sample from the posterior of a model with additional diagnositics
#' @export
sample_model <- function(model, data, scenario = data.table::data.table(id = 1),
                         diagnostics = TRUE, ...) {
  fit <- cmdstanr::cmdstan_model(model)$sample(data = data, ...)

  out <- scenario |>
    DT(, fit := list(fit))

  if (diagnostics) {
    diag <- fit$sampler_diagnostics(format = "df")
    diagnostics <- data.table(
      samples = nrow(diag),
      max_rhat = round(max(
        fit$summary(
          variables = NULL, posterior::rhat,
          .args = list(na.rm = TRUE)
        )$`posterior::rhat`,
        na.rm = TRUE
      ), 2),
      divergent_transitions = sum(diag$divergent__),
      per_divergent_transitions = sum(diag$divergent__) / nrow(diag),
      max_treedepth = max(diag$treedepth__)
    )
    diagnostics[, no_at_max_treedepth := sum(diag$treedepth__ == max_treedepth)]
    diagnostics[, per_at_max_treedepth := no_at_max_treedepth / nrow(diag)]
    out <- cbind(out, diagnostics)

    timing <- round(fit$time()$total, 1)
    out[, run_time := timing]
  }
  return(out[])
}

#' Add natural scale summary parameters for a lognormal distribution
#' @export
add_natural_scale_mean_sd <- function(dt) {
  nat_dt <- dt |>
    data.table::DT(, mean := exp(meanlog + sdlog ^ 2 / 2)) |>
    data.table::DT(,
     sd := exp(meanlog + (1 / 2) * sdlog ^ 2) * sqrt(exp(sdlog ^ 2) - 1)
    )
  return(nat_dt[])
}

#' Extract posterior samples for a lognormal brms model
#' @export
extract_lognormal_draws <- function(
  data, id_vars, from_dt = FALSE
) {
  if (from_dt) {
    draws <- data$fit[[1]]$draws(variables = c("Intercept", "Intercept_sigma"))
  }else {
    draws <- data
  }

  draws <- posterior::as_draws_df(draws) |>
    data.table::as.data.table()
  data.table::setnames(
    draws, c("Intercept", "Intercept_sigma"), c("meanlog", "sdlog"),
    skip_absent = TRUE
  )
  data.table::setnames(
    draws, c("b_Intercept", "b_sigma_Intercept"), c("meanlog", "sdlog"),
    skip_absent = TRUE
  )
  draws <- draws[, .(meanlog, sdlog)]
  draws <- draws[, sdlog := exp(sdlog)]
  draws <- add_natural_scale_mean_sd(draws)

  if (!missing(id_vars)) {
    draws <- merge(
      draws[, id := id_vars$id], id_vars, by = "id"
    )
  }
  return(draws[])
}

#' Convert posterior lognormal samples to long format
#' @export
draws_to_long <- function(draws) {
  long_draws <- data.table::melt(
    draws,
    measure.vars = c("meanlog", "sdlog", "mean", "sd"),
    variable.name = "parameter"
  )
  return(long_draws[])
}

#' Make posterior lognormal samples relative to true values
#' @export
make_relative_to_truth <- function(draws, secondary_dist, by = "parameter") {
  draws <- merge(
    draws,
    secondary_dist[, true_value := value][, value := NULL],
    by = by
  )

  draws <- draws[, rel_value := value / true_value]
  return(draws[])
}

#' Summarise lognormal posterior estimates
#' @export
summarise_lognormal_draws <- function(draws, sf, not_by = "value") {
  by_cols <- setdiff(
    colnames(draws), not_by
  )
  summarised_draws <- draws[,
    .(
      mean = mean(value),
      median = median(value),
      q2.5 = quantile(value, 0.025),
      q5 = quantile(value, 0.05),
      q20 = quantile(value, 0.2),
      q35 = quantile(value, 0.35),
      q65 = quantile(value, 0.65),
      q80 = quantile(value, 0.8),
      q95 = quantile(value, 0.95),
      q97.5 = quantile(value, 0.975)
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