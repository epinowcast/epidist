tar_group_by(
  distributions,
    data.table(
    scenario = c("short"),
    meanlog = c(1.2),
    sdlog = c(0.4)
  ) |>
    DT(, mean := exp(meanlog + sdlog^2/2)) |>
    DT(, sd := exp(meanlog + (1/2)*sdlog^2)*sqrt(exp(sdlog^2) - 1)),
  scenario
)
