tar_group_by(
  estimation_times,
  data.table(
    scenario = c("early outbreak", "near peak", "past peak", "late outbreak"),
    time = c(15, 30, 45, 60)
  ),
  scenario
)
