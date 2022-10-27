tar_stan_mcmc(
  model_fits, 
  stan_files = model_paths,
  data = do.call(
    models[[1]], 
    list(data = group_sampled_observations, fn = make_standata_target, 
         stan_model = model_stan_code[[1]])
  ),
  refresh = 0, init = 1, show_messages = FALSE,
  pattern = cross(map(models, model_stan_code), group_sampled_observations)
)
