tar_target(
  model_stan_code, 
  do.call(
    models[[1]],
    list(data = dummy_obs, fn = brms::make_stancode, save_model = model_paths)
  ),
  pattern = map(models, model_paths)
)
