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
      list(
        data = dummy_obs, fn = brms::make_stancode, save_model = model_path)
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
      list(
        data = list_observations[[1]], fn = brms::make_standata,
        data_cases = list_retro_incidence[[1]]
      )
    ),
    pattern = map(list_observations, list_retro_incidence)
  ),
  tar_target(
    fit, 
      sample_model(
        model = compiled_model_path,
        data = standata,
        scenario = scenarios,
        adapt_delta = 0.95,
        parallel_chains = parallel_chains,
        refresh = 0, 
        show_messages = FALSE,
        iter_sampling = 1000,
        seed = 123
      ),
    pattern = map(standata, scenarios),
    deployment = "worker"
  ),
  tar_file(
    save_diagnostics,
    save_csv(
      fit[, -c("fit")], paste0(model_name, ".csv"), path = "data/diagnostics"
    )
  ),
  tar_target(
    draws,
    fit |>
      extract_lognormal_draws(scenarios, from_dt = TRUE),
    pattern = map(fit, scenarios)
  ),
  tar_file(
    save_lognormal_draws,
    save_dataset(
      draws, path = paste0("data/posteriors/", model_name),
      partitioning = "id"
    )
  ),
  tar_target(
    summarised_draws,
    draws |> 
      draws_to_long() |>
      summarise_draws(sf = 2)
  ),
  tar_file(
    save_summarised_draws,
    save_csv(
      summarised_draws, paste0(model_name, ".csv"),
      path = "data/summarised_posteriors"
    )
  )
)
