data <- as_latent_individual(sim_obs)
fit <- epidist(data)
prep <- brms::prepare_predictions(fit)

stancode <- epidist(data, fn = brms::make_stancode)

# Won't be able to use expose in the package but can try with it for now
# Need some solution for how not to write code twice? Could expose and put in
# package precompiled somehow?
brms::expose_functions(x = fit, vectorize = TRUE)

# real latent_lognormal_lpdf(vector y, vector mu, vector sigma, vector pwindow,
#                            vector swindow, array[] real obs_t) {
#   int n = num_elements(y);
#   vector[n] d = y - pwindow + swindow;
#   vector[n] obs_time = to_vector(obs_t) - pwindow;
#   return lognormal_lpdf(d | mu, sigma) - lognormal_lcdf(obs_time | mu, sigma);
# }

i <- 1