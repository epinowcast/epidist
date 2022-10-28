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
      sample_model(
        model = compiled_model_path,
        data = standata,
        scenario = scenarios,
        adapt_delta = 0.95,
        refresh = 0, 
        show_messages = FALSE,
        seed = 123
      ),
    pattern = map(standata, scenarios)
  ),
  tar_target(
    diagnostics,
    fit[, fit := NULL]
  ),
  tar_file(
    save_diagnostics,
    save_csv(diagnostics, paste0("data/diagnostics/", model_name, '.csv'))
  ),
  tar_target(
    draws,
    extract_lognormal_draws(fit, scenarios),
    pattern = map(fit, scenarios)
  ),
  tar_file(
    save_lognormal_draws,
    save_csv(draws, paste0("data/posteriors/", model_name, '.csv'))
  ),
  tar_target(
    summarised_draws,
    summarise_lognormal_draws(draws)
  ),
  tar_file(
    save_summarised_draws,
    save_csv(summarised_draws, paste0("data/summarise_posteriors/", model_name, '.csv'))
  )
)
