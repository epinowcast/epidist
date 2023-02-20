# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation
library(forcats) # manipulate factors
library(scoringutils) # for scoring forecasts/posterior predictions

# Load case study data
outbreak_obs <- fread(here("data/scenarios/outbreak-simulation.csv"))

outbreak_long <- outbreak_obs |>
  DT(distribution == "long")

obs_window_vec <- c(15, 30, 45, 60)
type_vec <- c("real time", "retrospective")

paramdata <- expand.grid(obs_window_vec, type_vec)

summlist <- vector("list", nrow(paramdata))
drawlist <- vector("list", nrow(paramdata))

for (i in seq_len(nrow(paramdata))) {
  pp <- paramdata[i, ]

  obs_t <- pp[[1]]
  type <- pp[[2]]

  truncated_obs <- outbreak_long |>
    filter_obs_by_obs_time(obs_time = obs_t)

  if (type == "real time") {
    cases_by_window <- construct_cases_by_obs_window(
      outbreak_long,
      windows = c(obs_t)
    )

    data_cases <- cases_by_window |>
      DT(case_type == "primary") |>
      DT(obs_at == obs_t)
  } else {
    cases_by_window <- construct_cases_by_obs_window(
      outbreak_long,
      windows = c(max(outbreak_long$stime_upr))
    )

    data_cases <- cases_by_window |>
      DT(case_type == "primary") |>
      DT(time < obs_t)
  }

  bfit <- backward_delay(
    data = truncated_obs, data_cases = data_cases,
    cores = 4
  )

  ss <- bfit$summary()

  ss2 <- ss |>
    as.data.table() |>
    DT(grepl("backward", variable)) |>
    DT(, time := 0:(obs_t - 1)) |>
    DT(, obs_t := obs_t) |>
    DT(, type := type)

  bfit_summ <- bfit$draws(
    variables = c("Intercept", "Intercept_sigma"), format = c("draws_matrix")
  ) |>
    as.data.table() |>
    mutate(
      meanlog = Intercept,
      sdlog = exp(Intercept_sigma),
      Mean = exp(meanlog + sdlog^2 / 2) / exp(1.8 + 0.8^2 / 2),
      Sd = sqrt((exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2)) /
       sqrt((exp(0.8^2) - 1) * exp(2 * 1.8 + 0.8^2))
    )

  bfit_summ2 <- bfit_summ |>
    select(Mean, Sd) |>
    melt() |>
    DT(, obs_t := obs_t) |>
    DT(, type := type)

  summlist[[i]] <- ss2
  drawlist[[i]] <- bfit_summ2
}

backward_simulation_summ <- summlist |> bind_rows()
backward_simulation_draw <- drawlist |> bind_rows()

save(
  "backward_simulation_summ", "backward_simulation_draw",
  file = here("scripts/simulations", "backward_simulation.rda")
)
