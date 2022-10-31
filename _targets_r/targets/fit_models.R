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
        parallel_chains = parallel_chains
        refresh = 0, 
        show_messages = FALSE,
        iter_sampling = 1000,
        seed = 123
      ),
    pattern = map(standata, scenarios)
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
      extract_lognormal_draws(scenarios, from_dt = TRUE) |>
      draws_to_long(),
    pattern = map(fit, scenarios)
  ),
  tar_file(
    save_lognormal_draws,
    save_csv(draws, paste0(model_name, ".csv"), path = "data/posteriors")
  ),
  tar_target(
    summarised_draws,
    summarise_lognormal_draws(draws, sf = 2)
  ),
  tar_file(
    save_summarised_draws,
    save_csv(
      summarised_draws, paste0(model_name, ".csv"),
      path = "data/summarised_posteriors"
    )
  )
)
