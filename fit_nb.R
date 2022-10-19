library(brms)
library(cmdstanr)
load("data_exponential.rda")
## I can't run brms on my mac so I changed to my pc
## but I can't use make on my pc so I'm doing this clunky thing...
## feel free to change up however you want

options(mc.cores=4)

truncated_obs$delay_daily <- truncated_obs$stime_daily-truncated_obs$ptime_daily
truncated_obs$obs_time_daily <- 20-truncated_obs$ptime_daily

## goes absolutely crazy and doesn't want to fit without taking eternity
rtrunc_model_nb <- brm(
  bf(delay_daily | trunc(lb=0, ub=obs_time_daily) ~ -1 + as.factor(r), shape ~ -1 + as.factor(r)), data = truncated_obs, family = negbinomial(),
  backend = "cmdstanr"
)

# save("rtrunc_model_nb",
#      file="fit_nb.rda")
