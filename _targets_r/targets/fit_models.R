tar_map(
  values = list(
    model_name = machine_model_names,
    model = models
  ),
  names = model_name,
  tar_file(
    model_path,
    paste0(
      "data/models/", model_name, ".stan"
    ) |>
      fs::file_create()
  ),
  tar_target(
    model_stan_code, 
    do.call(
      model,
      list(data = dummy_obs, fn = brms::make_stancode, save_model = model_path)
    )
  ),
  tar_file(
    compiled_model_path,
    {
      stan_code <- model_stan_code
      cmdstan_model(model_path)$stan_file()
    }
  ),
  tar_target(
    standata,
    do.call(
      model,
      list(data = list_observations[[1]], fn = brms::make_standata)
    ),
    pattern = map(list_observations)
  ),
  tar_target(
    fit, 
    cmdstan_model(compiled_model_path)$sample(
      data = standata,
      adapt_delta = 0.95,
      seed = 123
    ),
    pattern = map(standata)
  )
)
