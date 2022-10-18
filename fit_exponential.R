library(brms)
library(cmdstanr)
load("data_exponential.rda")
## I can't run brms on my mac so I changed to my pc
## but I can't use make on my pc so I'm doing this clunky thing...
## feel free to change up however you want

options(mc.cores=4)

# Fit lognormal model with no corrections
naive_model <- brm(
  bf(delay ~ -1 + as.factor(r), sigma ~ -1 + as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

# right truncation only (if we're looking forward in time)
rtrunc_model <- brm(
  bf(delay | trunc(lb=0, ub=obs_time) ~ -1 + as.factor(r), sigma ~ -1 + as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

# left truncation only (if we're looking backward in time)
ltrunc_model <- brm(
  bf(delay | trunc(lb=0, ub=stime) ~ -1 + as.factor(r), sigma ~ -1 + as.factor(r)), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)

save("naive_model",
     "rtrunc_model",
     "ltrunc_model",
     file="fit_exponential.rda")
