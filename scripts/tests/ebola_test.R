library(dynamicaltruncation)
library(here)
library(data.table)
library(ggplot2); theme_set(theme_bw())

raw_case_study_data <- fread(here("data-raw", "pnas.1518587113.sd02.csv"))

case_study_data <- raw_case_study_data |>
  DT(,
     .(
       id = 1:.N, onset_date = lubridate::dmy(`Date of symptom onset`),
       test_date = lubridate::dmy(`Date of sample tested`)
     )
  ) |>
  DT(, `:=`( ## FIXME: THIS IS DEFINITELY PROBLEMATIC
    ptime = as.numeric(onset_date - min(onset_date)),
    stime = as.numeric(test_date - min(onset_date))
  )
  ) |>
  observe_process()

ggplot(case_study_data) +
  geom_smooth(aes(ptime, stime-ptime))

case_study_data_trunc <- case_study_data |>
  filter_obs_by_obs_time(
    obs_time = 240
  ) |>
  DT(, obs_type := "real-time")

case_study_data_retro <- case_study_data |>
  filter_obs_by_ptime(
    obs_time = 240,
    obs_at = "max_secondary"
  ) |>
  DT(, obs_type := "retrospective")


mean(case_study_data_trunc$stime-case_study_data_trunc$ptime)
mean(case_study_data_retro$stime-case_study_data_retro$ptime)

naive_trunc <- naive_delay(
  data = case_study_data_trunc, cores = 4, refresh = 0
)

naive_retro <- naive_delay(
  data = case_study_data_retro, cores = 4, refresh = 0
)

naive_trunc_draws <- extract_lognormal_draws(naive_trunc)
naive_retro_draws <- extract_lognormal_draws(naive_retro)

plot(density(naive_trunc_draws$mean))
lines(density(naive_retro_draws$mean), col=2)
