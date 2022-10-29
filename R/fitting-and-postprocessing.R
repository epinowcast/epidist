sample_model <- function(model, data, scenario, diagnostics = TRUE, ...) {
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

extract_lognormal_draws <- function(
  data, id_vars, variables = c("Intercept", "Intercept_sigma")
) {
  draws <- data$fit[[1]]$draws(variables = variables)
  draws <- posterior::as_draws_df(draws) |>
    data.table::as.data.table()
  data.table::setnames(
    draws, c("Intercept", "Intercept_sigma"), c("meanlog", "log_sdlog")
  )
  draws <- draws |>
    data.table::DT(, sdlog := exp(log_sdlog)) |>
    data.table::DT(, mean := exp(meanlog + sdlog ^ 2 / 2)) |>
    data.table::DT(,
     sd := exp(meanlog + (1 / 2) * sdlog ^ 2) * sqrt(exp(sdlog ^ 2) - 1)
    )

  if (!missing(id_vars)) {
    draws <- merge(
      draws[, id := id_vars$id], id_vars, by = "id"
    )
  }
  return(draws[])
}

summarise_lognormal_draws <- function(draws) {
  by_cols <- setdiff(
    colnames(draws),
    c("meanlog", "log_sdlog", "sdlog", "mean", "sd", ".chain", ".iteration", ".draw")
  )
  long_draws <- melt(
    draws,
    measure.vars = c("meanlog", "log_sdlog", "sdlog", "mean", "sd"),
    variable.name = "parameter"
  )

  summarised_draws <- long_draws[,
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
    by = c("parameter", by_cols)
  ]

  return(summarised_draws[])
}