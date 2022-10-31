tar_group_by(
  distributions,
  data.table(
    scenario = c("short", "medium", "long"),
    meanlog = c(1.2, 1.6, 1.8),
    sdlog = c(0.4, 0.6, 0.8)
  ) |>
    add_natural_scale_mean_sd()
)
