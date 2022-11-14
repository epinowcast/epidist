tar_file(
  save_models,
  data.table(
    model = names(models), in_code = machine_model_names
  ) |>
    save_csv("models.csv", path = "data/meta")
)
