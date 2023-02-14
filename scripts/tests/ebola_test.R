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
  geom_smooth(aes(ptime, delay_daily))

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

sd(case_study_data_trunc$stime-case_study_data_trunc$ptime)
sd(case_study_data_retro$stime-case_study_data_retro$ptime)

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

tar_load("sampled_ebola_observations")
sampled_ebola_observations_240 <- sampled_ebola_observations %>%
  filter(scenario=="240 days")


sampled_ebola_observations_240_rt <- sampled_ebola_observations_240 %>%
  filter(obs_type=="real-time")

sampled_ebola_observations_240_ret <- sampled_ebola_observations_240 %>%
  filter(obs_type=="retrospective")

mean(sampled_ebola_observations_240_rt$stime-sampled_ebola_observations_240_rt$ptime)
mean(sampled_ebola_observations_240_ret$stime-sampled_ebola_observations_240_ret$ptime)

sd(sampled_ebola_observations_240_rt$stime-sampled_ebola_observations_240_rt$ptime)
sd(sampled_ebola_observations_240_ret$stime-sampled_ebola_observations_240_ret$ptime)

plot(density(case_study_data_trunc$stime-case_study_data_trunc$ptime), lwd=2)
lines(density(sampled_ebola_observations_240_rt$stime-sampled_ebola_observations_240_rt$ptime), col=2, lwd=2)

naive_retro_sub <- naive_delay(
  data = sampled_ebola_observations_240_ret, cores = 4, refresh = 0
)

naive_retro_sub_draws <- extract_lognormal_draws(naive_retro_sub)

hist(sampled_ebola_observations_240_ret$delay_daily, breaks=50, freq=FALSE)
curve(dlnorm(x, meanlog=1.59, sdlog=exp(-0.17)), add=TRUE)
