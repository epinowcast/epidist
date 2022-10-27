tar_target(growth_rate, {
  data.table(
    time = 0:59,
    r = c(rep(0.2, 20), 0.2 - 0.02 * 1:20, rep(-0.2, 20))
  )
})
