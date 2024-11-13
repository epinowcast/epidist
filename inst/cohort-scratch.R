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
  filter_obs_by_obs_time(obs_time = obs_time) |>
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

primarycensored_lognormal_uniform_lcdf <- brms::custom_family(
  "primarycensored_lognormal_uniform_lcdf",
  dpars = lognormal$dpar,
  links = c(lognormal$link, lognormal$link_sigma),
  type = lognormal$type,
  loop = TRUE,
  vars = c("pwindow", "vreal1[n]")
)

primarycensored_lognormal_uniform_lcdf_file <- file.path(
  tempdir(), "primarycensored_lognormal_uniform_lcdf.stan"
)

data <- cohort_obs |>
  select(d = delay, n = n) |>
  mutate(
    pwindow = 1,
    q = pmax(d - pwindow, 0)
  )
  
pcd_function <- pcd_load_stan_functions("primarycensored_lognormal_uniform_lcdf")
pcd_function <- sub(pattern = "array\\[\\] real params", "real mu, real sigma", pcd_function)
pcd_function <- gsub("\\s*real mu = params\\[1\\];\\n\\s*real sigma = params\\[2\\];\\n", "", pcd_function)

stanvars_functions <- brms::stanvar(
  block = "functions",
  scode = pcd_function
)

# stanvars_tparameters <- brms::stanvar(
#   block = "tparameters",
#   scode = .stan_chunk("cohort_model/tparameters.stan")
# )

# stanvars_tdata <- brms::stanvar(
#   block = "tdata",
#   scode = .stan_chunk("cohort_model/tdata.stan")
# )

pwindow <- data$pwindow

stanvars_data <- brms::stanvar(
  x = pwindow,
  block = "data",
  scode = .stan_chunk("cohort_model/data.stan")
)

stanvars_all <- stanvars_functions + stanvars_data

brms::make_stancode(
  formula = d | weights(n) + vreal(q) ~ 1,
  family = primarycensored_lognormal_uniform_lcdf,
  data = data,
  stanvars = stanvars_all,
)

fit_pcd <- brms::brm(
  formula = d | weights(n), vreal(q) ~ 1,
  family = primarycensored_lognormal_uniform_lcdf,
  data = data,
  stanvars = stanvars_all,
)
