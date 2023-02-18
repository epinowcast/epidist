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
  DT(distribution=="long")

obs_window_vec <- c(15, 30, 45, 60)
type_vec <- c("real time", "retrospective")

paramdata <- expand.grid(obs_window_vec, type_vec)

reslist <- vector('list', nrow(paramdata))

for (i in 1:nrow(paramdata)) {
  pp <- paramdata[i,]
  
  obs_t <- pp[[1]]
  type <- pp[[2]]
  
  truncated_obs <- outbreak_long |>
    filter_obs_by_obs_time(obs_time = obs_t)
  
  if (type=="real time") {
    cases_by_window <- construct_cases_by_obs_window(
      outbreak_long, windows = c(obs_t)
    )
    
    data_cases <- cases_by_window |>
      DT(case_type=="primary") |>
      DT(obs_at==obs_t)
  } else {
    cases_by_window <- construct_cases_by_obs_window(
      outbreak_long, windows = c(max(outbreak_long$stime_upr))
    )
    
    data_cases <- cases_by_window |>
      DT(case_type=="primary") |>
      DT(time<=obs_t)
    
  }
  
  bfit <- backward_delay(data=truncated_obs, data_cases=data_cases, cores=4)
  
  reslist[[i]] <- bfit
}

backward_simulation <- reslist

save("backward_simulation", file="backward_simulation.rda")
