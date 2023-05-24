tar_file(
  save_models,
  data.table(
    model = names(models), in_code = machine_model_names
  ) |>
    rbind(data.table(
      model = "Joint incidence and delay estimation",
      in_code = "epinowcast"
    )) |>
    save_csv("models.csv", path = "data/meta")
)
