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
type_vec <- c("Real-time", "Real-time (filtered)", "Retrospective")

paramdata <- expand.grid(obs_window_vec, type_vec)

summlist <- vector("list", nrow(paramdata))
drawlist <- vector("list", nrow(paramdata))

for (i in seq_len(nrow(paramdata))) {
  pp <- paramdata[i, ]

  obs_t <- pp[[1]]
  type <- pp[[2]]
  message("Estimating ", type, " model for ", obs_t, " days")

  truncated_obs <- outbreak_long |>
    filter_obs_by_obs_time(obs_time = obs_t)

  if (type == "Real-time") {
    bfit <- dynamical_censoring_adjusted_delay(
      data = truncated_obs,
      cores = 4
    )

    filter_t <- obs_t
  } else {
    if (type == "Real-time (filtered)") {
      window <- obs_t
      filter_t <- obs_t - 10
      delay_obs <- outbreak_long |>
        filter_obs_by_obs_time(obs_time = filter_t)
    }else {
      window <- max(outbreak_long$stime_upr)
      delay_obs <- truncated_obs
      filter_t <- obs_t
    }
    cases_by_window <- construct_cases_by_obs_window(
      outbreak_long,
      windows = c(window)
    )

    data_cases <- cases_by_window |>
      DT(case_type == "primary") |>
      DT(time < filter_t)

    bfit <- dynamical_censoring_adjusted_delay(
      data = delay_obs,
      data_cases = data_cases,
      cores = 4
    )
  }

  ss <- posterior::summarise_draws(bfit)

  ss2 <- ss |>
    as.data.table() |>
    DT(grepl("backward", variable)) |>
    DT(, time := 0:(filter_t - 1)) |>
    DT(, obs_t := obs_t) |>
    DT(, type := type)

  bfit_summ <- extract_lognormal_draws(bfit)

  bfit_summ2 <- bfit_summ |>
    select(Mean = mean, Sd = sd) |>
    melt() |>
    DT(, obs_t := obs_t) |>
    DT(, type := type)

  summlist[[i]] <- ss2
  drawlist[[i]] <- bfit_summ2
}

backward_simulation_summ <- summlist |>
 bind_rows()
backward_simulation_draw <- drawlist |>
 bind_rows()

save(
  "backward_simulation_summ", "backward_simulation_draw",
  file = here("scripts/simulations", "dynamical_simulation.rda")
)
