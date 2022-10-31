library(targets)
library(tarchetypes)
library(stantargets)
library(cmdstanr)
library(data.table)
library(ggplot2)
library(purrr, quietly = TRUE)
library(here)
library(future)
library(future.callr)
plan(callr)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")
set_cmdstan_path()  

# Set the number of chains to run in parallel (more than 4 will have no impact
# on runtimes)
parallel_chains <- 4

tar_option_set(
  packages = c("data.table", "ggplot2", "purrr", "cmdstanr", "brms", "here"),
  deployment = "worker",
  memory = "transient",
  workspace_on_error = TRUE,
  error = "continue",
  garbage_collection = TRUE
)
