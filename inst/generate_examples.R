source("tests/testthat/setup.R")
set.seed(1)
prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(prep_obs, seed = 1)
saveRDS(fit, "inst/extdata/fit.rds")
prep_obs_gamma <- as_latent_individual(sim_obs_gamma)
fit_gamma <- epidist(
  prep_obs_gamma,
  family = stats::Gamma(link = "log"),
  seed = 1
)
saveRDS(fit_gamma, "inst/extdata/fit_gamma.rds")
