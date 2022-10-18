library(brms)

# Fit lognormal model with no corrections
naive_model <- brm(
  bf(daily_delay ~ 1, sigma ~ 1), data = truncated_obs, family = lognormal(),
  backend = "cmdstanr"
)
