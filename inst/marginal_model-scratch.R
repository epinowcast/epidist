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
  mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_lwr + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_lwr + 1,
    obs_time = obs_time,
    delay = stime_lwr - ptime_lwr
  ) |>
  filter(.data$stime_upr <= .data$obs_time) |>
  filter(.data$stime_upr <= obs_time) |>
  dplyr::slice_sample(n = sample_size, replace = FALSE)

# Create cohort version of data

cohort_obs <- sim_obs |>
  group_by(delay) |>
  summarise(n = n())

ggplot(cohort_obs, aes(x = delay, y = n)) +
  geom_col()

fit_direct <- brms::brm(
  formula = delay ~ 1,
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
  "marginal_lognormal",
  dpars = lognormal$dpar,
  links = c(lognormal$link, lognormal$link_sigma),
  type = "int",
  loop = TRUE,
  vars = c("vreal1[n]")
)

data <- cohort_obs |>
  select(d = delay, n = n) |>
  mutate(pwindow = 1)

pcd_stanvars_functions <- brms::stanvar(
  block = "functions",
  scode = pcd_load_stan_functions()
)

stanvars_functions <- brms::stanvar(
  block = "functions",
  scode = .stan_chunk(file.path("marginal_model", "functions.stan"))
)


stanvars_functions[[1]]$scode <- gsub(
  "family", "lognormal", stanvars_functions[[1]]$scode,
  fixed = TRUE
)

stanvars_functions[[1]]$scode <- gsub(
  "input_dist_id", 1, stanvars_functions[[1]]$scode,
  fixed = TRUE
)

stanvars_functions[[1]]$scode <- gsub(
  "dpars_A", "real mu, real sigma",
  stanvars_functions[[1]]$scode,
  fixed = TRUE
)

stanvars_functions[[1]]$scode <- gsub(
  "dpars_1", "mu",
  stanvars_functions[[1]]$scode,
  fixed = TRUE
)

stanvars_functions[[1]]$scode <- gsub(
  "dpars_2", "sigma",
  stanvars_functions[[1]]$scode,
  fixed = TRUE
)

stanvars_all <- pcd_stanvars_functions + stanvars_functions

stancode <- brms::make_stancode(
  formula = d | weights(n) + vreal(pwindow) ~ 1,
  family = primarycensored_family,
  data = data,
  stanvars = stanvars_all,
)

fit_pcd <- brms::brm(
  formula = d | weights(n) + vreal(pwindow) ~ 1,
  family = primarycensored_family,
  data = data,
  stanvars = stanvars_all,
  backend = "cmdstanr"
)
