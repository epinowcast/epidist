#' Sample from the posterior of a model with additional diagnositics
#' @export
sample_model <- function(model, data, scenario = data.table::data.table(id = 1),
                         diagnostics = TRUE, ...) {

  out <- scenario |>
    copy()

  # Setup failure tolerant model fitting
  fit_model <- function(model, data, ...) {
    fit <- cmdstanr::cmdstan_model(model)$sample(data = data, ...)
    print(fit)
    return(fit)
  }
  safe_fit_model <- purrr::safely(fit_model)
  fit <- safe_fit_model(model, data, ...)

  if (!is.null(fit$error)) {
    out <- out |>
      DT(, error := list(fit$error[[1]]))
    diagnostics <- FALSE
  }else {
    out <- out |>
      DT(, fit := list(fit$result))
    fit <- fit$result
  }

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

#' Sample from the posterior of an epinowcast model with additional diagnositics
#' @export
sample_epinowcast_model <- function(model, data,
                                   scenario = data.table::data.table(id = 1),diagnostics = TRUE, ...) {

  out <- scenario |>
    copy()

  # Setup failure tolerant model fitting
  fit_model <- function(model, data, ...) {
    fit <- epinowcast::enw_model(
      target_dir = here::here("data", "models")
    )$sample(data = data, ...)
    print(fit)
    return(fit)
  }
  safe_fit_model <- purrr::safely(fit_model)
  fit <- safe_fit_model(model, data, ...)

  if (!is.null(fit$error)) {
    out <- out |>
      DT(, error := list(fit$error[[1]]))
    diagnostics <- FALSE
  }else {
    out <- out |>
      DT(, fit := list(fit$result))
    fit <- fit$result
  }

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
    if (!any(colnames(data) %in% "fit")) {
      return(id_vars[])
    }
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

#' Extract posterior samples for a lognormal epinowcast model
#' @export
extract_epinowcast_draws <- function(
  data, id_vars, from_dt = FALSE
) {
  if (from_dt) {
    if (!any(colnames(data) %in% "fit")) {
      return(id_vars[])
    }
    draws <- data$fit[[1]]$draws(
      variables = c("refp_mean_int[1]", "refp_sd_int[1]"), format = "draws_df"
    )
  }else {
    draws <- data$fit[[1]]$draws(
      variables = c("refp_mean_int[1]", "refp_sd_int[1]"), format = "draws_df"
    )
  }

  draws <- draws |>
    data.table::setDT()

  data.table::setnames(
    draws, c("refp_mean_int[1]", "refp_sd_int[1]"), c("meanlog", "sdlog"),
    skip_absent = TRUE
  )
  draws <- draws[, .(meanlog, sdlog)]
  draws <- add_natural_scale_mean_sd(draws)

  if (!missing(id_vars)) {
    draws <- merge(
      draws[, id := id_vars$id], id_vars, by = "id"
    )
  }
  return(draws[])
}

#' Primary event bias correction
#' @export
primary_censoring_bias_correction <- function(draws) {
  draws <- draws |>
    DT(, mean := mean + runif(.N, min = 0, max = 1)) |>
    DT(, meanlog := log(mean^2 / sqrt(sd^2 + mean^2))) |>
    DT(, sdlog := sqrt(log(1 + (sd^2 / mean^2))))
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

#' Summarise posterior draws
#' @param not_by A vector of columns to exclude from the grouping
#' This will be overridden if by is specified.
#' @inheritParams summarise_variable
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
    .(
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
#' @param draws A data.table of posterior draws
#' @param sf The number of significant figures to use
#' @param variable The variable to summarise
#' @param by A vector of columns to group by
#' @export
summarise_variable <- function(draws, variable, sf = 6, by = c()) {
  if (missing(variable)) {
    stop("variable must be specified")
  }
  summarised_draws <- draws |>
    copy() |>
    DT(, value := variable, env = list(variable = variable)) |>
    summarise_draws(sf = sf, by = by)
  return(summarised_draws[])
}