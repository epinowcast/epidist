source("tests/testthat/setup.R")
prep_obs <- as_latent_individual(sim_obs)
fit <- epidist(prep_obs)
saveRDS(fit, "inst/extdata/fit.rds")
