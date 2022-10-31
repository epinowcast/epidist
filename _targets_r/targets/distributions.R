tar_group_by(
  distributions,
  data.table(
    scenario = c("medium"),
    meanlog = c(1.6),
    sdlog = c(0.6)
  ) |>
    add_natural_scale_mean_sd()
  scenario
)
