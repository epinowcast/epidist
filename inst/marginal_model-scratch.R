library(dplyr)
library(ggplot2)
library(brms)
library(primarycensored)
library(bayesplot)

set.seed(101)

obs_time <- 25
sample_size <- 500

meanlog <- 1.8
sdlog <- 0.5

sim_obs <- simulate_gillespie() |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  observe_process() |>
  filter(.data$stime_upr <= obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Create cohort version of data

cohort_obs <- sim_obs |>
  group_by(delay = delay_daily) |>
  summarise(n = n())

ggplot(cohort_obs, aes(x = delay, y = n)) +
  geom_col()

fit_direct <- brms::brm(
  formula = delay_daily ~ 1,
  family = "lognormal",
  data = sim_obs
)

summary(fit_direct)

fit_direct_weighted <- brms::brm(
  formula = delay | weights(n) ~ 1,
  family = "lognormal",
  cohort_obs,
)

summary(fit_direct_weighted)

lognormal <- brms::lognormal()

primarycensored_family <- brms::custom_family(
  "primarycensored_wrapper",
  dpars = lognormal$dpar,
  links = c(lognormal$link, lognormal$link_sigma),
  type = "int",
  loop = TRUE,
  vars = c("vreal1[n]", "pwindow[n]")
)

data <- cohort_obs |>
  select(d = delay, n = n) |>
  mutate(
    pwindow = 1,
    q = pmax(d - pwindow, 0)
  )

pcd_stanvars_functions <- brms::stanvar(
  block = "functions",
  scode = pcd_load_stan_functions()
)

stanvars_functions <- brms::stanvar(
  block = "functions",
  scode = .stan_chunk("cohort_model/functions.stan")
)

pwindow <- data$pwindow

stanvars_data <- brms::stanvar(
  x = pwindow,
  block = "data",
  scode = .stan_chunk("cohort_model/data.stan")
)

stanvars_all <- pcd_stanvars_functions + stanvars_functions + stanvars_data

stancode <- brms::make_stancode(
  formula = d | weights(n) + vreal(q) ~ 1,
  family = primarycensored_family,
  data = data,
  stanvars = stanvars_all,
)

model <- rstan::stan_model(model_code = stancode)

fit_pcd <- brms::brm(
  formula = d | weights(n) + vreal(q) ~ 1,
  family = primarycensored_family,
  data = data,
  stanvars = stanvars_all,
  backend = "cmdstanr"
)
