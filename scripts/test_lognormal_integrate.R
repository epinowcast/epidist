library(rstan)
library(cmdstanr)

model <- cmdstan_model("scripts/lognormal_integrate.stan")

standata <- list(
  r=0.2,
  mu=1,
  sigma=0.5,
  max_delay=20
)

mysample <- model$sample(data=standata, chains=1, iter_warmup=1, iter_sampling=1)

mysample

log(integrate(function(x) {
  exp(dlnorm(x, meanlog=1, sdlog=0.5, log=TRUE) - 0.2 * x)
}, 0, 20)[[1]])
