library(brms)
library(cmdstanr)
load("data_exponential.rda")
## I can't run brms on my mac so I changed to my pc
## but I can't use make on my pc so I'm doing this clunky thing...
## feel free to change up however you want

# Fit lognormal model with no corrections
naive_model <- brm(
  bf(delay ~ as.factor(r), sigma ~ as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

# right truncation only (if we're looking forward in time)
rtrunc_model <- brm(
  bf(delay | trunc(lb=0, ub=obs_time) ~ as.factor(r), sigma ~ as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

# left truncation only (if we're looking backward in time)
ltrunc_model <- brm(
  bf(delay | trunc(lb=0, ub=obs_delay) ~ as.factor(r), sigma ~ as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

save("naive_model",
     "rtrunc_model",
     "ltrunc_model",
     file="fit_exponential.rda")
