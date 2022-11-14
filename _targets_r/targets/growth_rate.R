tar_target(growth_rate, {
  data.table(
      r = c(-0.2, -0.1, 0, 0.1, 0.2),
      scenario = c("fast decay", "decay", "stable", "growth", "fast growth")
    )
})
