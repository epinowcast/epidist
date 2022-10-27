tar_target(
  model_paths,
  paste0(
    "data/models/", gsub(" ", "_", tolower(names(models))), ".stan"
  ) |>
    fs::file_create(),
  format = "file",
  pattern = map(models)
)
