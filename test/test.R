library(data.table)
library(purrr, quietly = TRUE)
library(here)
library(brms)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)

outbreak <- simulate_gillespie(seed=101)

truncated_obs <- outbreak |>
  simulate_secondary(
    meanlog = 1.8,
    sdlog = 0.3
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = 25)

standata <- list(
  N=nrow(truncated_obs),
  Y=truncated_obs$delay_daily,
  prior_only=FALSE
)

fit <- sample_model("../data/models/naive.stan", data=standata,
                    scenario=data.frame(dummy=1),
                    diagnostics = FALSE)

draws <- fit$fit[[1]]$draws(variables = c("Intercept", "Intercept_sigma"))
draws <- posterior::as_draws_df(draws) |>
  data.table::as.data.table()
data.table::setnames(
  draws, c("Intercept", "Intercept_sigma"), c("meanlog", "sdlog")
)

draws |>
  data.table::DT(, sdlog := exp(sdlog)) |>
  data.table::DT(, mean := exp(meanlog + sdlog ^ 2 / 2)) |>
  data.table::DT(,
                 sd := exp(meanlog + (1 / 2) * sdlog ^ 2) * sqrt(exp(sdlog ^ 2) - 1)
  )

