tar_group_by(
  group_truncated_ebola_obs,
  rbindlist(truncated_ebola_obs, retrospective_ebola_obs),
  scenario, obs_type
)
