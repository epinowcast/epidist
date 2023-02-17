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

obs_window <- 60

truncated_obs <- outbreak_long |>
  filter_obs_by_obs_time(obs_time = obs_window)

cases_by_window <- construct_cases_by_obs_window(
  outbreak_long, windows = c(obs_window)
)

data_cases <- cases_by_window |>
  DT(case_type=="primary") |>
  DT(obs_at==obs_window)

bfit <- backward_delay(data=truncated_obs, data_cases=data_cases, cores=4)

backward <- truncated_obs |>
  copy() |>
  DT(, .(mean = mean(delay_daily)), by = stime_daily)

ss <- bfit$summary()

ss2 <- ss |>
  as.data.table() |>
  DT(grepl("backward", variable)) |>
  DT(, time := 0:59)

g1 <- ggplot(backward) +
  geom_point(aes(stime_daily, mean)) +
  geom_ribbon(data = ss2, aes(time, ymin = q5, ymax = q95), alpha = 0.3) +
  geom_line(data = ss2, aes(time, mean)) +
  scale_x_continuous("Secondary event time (days)") +
  scale_y_continuous("Mean backward delay (days)") +
  theme_bw()

bfit_summ <- bfit$draws(variables = c("Intercept", "Intercept_sigma"), format=c("draws_matrix")) |>
  as.data.table() |>
  mutate(
    meanlog=Intercept,
    sdlog=exp(Intercept_sigma),
    Mean=exp(meanlog + sdlog^2/2)/exp(1.8 + 0.8^2/2),
    Sd=sqrt((exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2))/sqrt((exp(0.8^2)-1)*exp(2*1.8+0.8^2))
  )

bfit_summ2 <- bfit_summ |>
  select(Mean, Sd) |>
  melt()

g2 <-ggplot(bfit_summ2) +
  geom_density(aes(value)) +
  scale_x_continuous("Relative to ground truth") +
  scale_y_continuous("Density") +
  facet_wrap(~variable) +
  theme_bw()

gcomb <- g1 + g2 +
  plot_layout(nrow=2)


# Save combined plots
ggsave(
  here("figures", "backward.png"), gcomb,
  height = 10, width = 8, dpi = 330
)
