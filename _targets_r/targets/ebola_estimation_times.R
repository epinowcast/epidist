tar_group_by(
  ebola_estimation_times,
  data.table(
    scenario = as.factor(c("60 days", "120 days", "180 days", "240 days")),
    time = c(60, 120, 180, 240)
  ),
  scenario
)
