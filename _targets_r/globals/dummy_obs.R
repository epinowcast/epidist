dummy_obs <- data.table::data.table(
  ptime = 1, stime = 2, delay_daily = 1, delay_lwr = 1, delay_upr = 2, 
  ptime_lwr = 1, ptime_upr = 2, stime_lwr = 1, stime_upr = 2, obs_at = 100,
  censored = "interval", censored_obs_time = 10, ptime_daily = 1,
  stime_daily = 1
)
