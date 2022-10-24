tar_target(distributions, {
  data.table(
    scenario = c("short", "medium", "long"),
    meanlog = c(1.2, 1.6, 2),
    sdlog = c(0.4, 0.6, 0.8)
  ) |>
    DT(, mean := exp(meanlog + sdlog^2/2)) |>
    DT(, sd := exp(meanlog + (1/2)*sdlog^2)*sqrt(exp(sdlog^2) - 1))
})
