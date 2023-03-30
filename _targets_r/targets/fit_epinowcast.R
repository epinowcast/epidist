list(
  tar_file(
    epinowcast_path,
    paste0(
      "data/models/", "epinowcast", ".stan"
    ) |>
      fs::file_create()
  ),
  tar_file(
    epinowcast_compiled_model_path,
    {
      stan_path <- epinowcast_path
      stan_code <- epinowcast::enw_model(
        target_dir = here::here("data/models")
      )
      stan_code$stan_file()
    }
  ),
  tar_target(
    epinowcast_fit, 
      epinowcast_delay(
        model = epinowcast_compiled_model_path,
        data = list_observations[[1]],
        max_delay = 30,
        scenario = scenarios,
        adapt_delta = 0.95,
        parallel_chains = parallel_chains,
        refresh = 0, 
        show_messages = FALSE,
        iter_sampling = 1000,
        seed = 123,
        sampler = sample_epinowcast_model
      ),
    pattern = map(list_observations, scenarios),
    deployment = "worker"
  ),
  tar_file(
    epinowcast_save_diagnostics,
    save_csv(
      epinowcast_fit[, -c("fit")], "epinowcast.csv", path = "data/diagnostics"
    )
  ),
  tar_target(
    epinowcast_draws,
    epinowcast_fit |>
      extract_epinowcast_draws(scenarios, from_dt = TRUE) |>
      primary_censoring_bias_correction(),
    pattern = map(epinowcast_fit, scenarios)
  ),
  tar_file(
    epinowcast_save_lognormal_draws,
    save_dataset(
      epinowcast_draws, path = paste0("data/posteriors/", "epinowcast"),
      partitioning = "id"
    )
  ),
  tar_target(
    epinowcast_summarised_draws,
    epinowcast_draws |> 
      draws_to_long() |>
      summarise_draws(sf = 2)
  ),
  tar_file(
    epinowcast_save_summarised_draws,
    save_csv(
      epinowcast_summarised_draws, "epinowcast.csv",
      path = "data/summarised_posteriors"
    )
  )
)
