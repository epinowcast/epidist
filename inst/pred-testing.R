data <- as_latent_individual(sim_obs)
fit <- epidist(data)
prep <- brms::prepare_predictions(fit)

stancode <- epidist(data, fn = brms::make_stancode)

# Won't be able to use expose in the package but can try with it for now
# Need some solution for how not to write code twice? Could expose and put in
# package precompiled somehow?
brms::expose_functions(x = fit, vectorize = TRUE)

i <- 1

