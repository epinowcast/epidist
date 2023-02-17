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
  DT(, `:=`(
    ptime = as.numeric(onset_date - min(onset_date)),
    stime = as.numeric(test_date - min(onset_date))
  )
  ) |>
  observe_process()

case_study_data_trunc <- case_study_data |>
  filter_obs_by_obs_time(
    obs_time = 60
  ) |>
  DT(, obs_type := "real-time")

case_study_data_retro <- case_study_data |>
  filter_obs_by_ptime(
    obs_time = 60,
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

hist(case_study_data_trunc$delay_daily, freq=FALSE)
curve(dlnorm(x, 1.47, exp(0.11)), add=T)

case_study_data_trunc2 <- case_study_data_trunc
case_study_data_trunc2 <- case_study_data_trunc2[case_study_data_trunc2$delay_daily!=0,]

naive_trunc2 <- naive_delay(
  data = case_study_data_trunc2, cores = 4, refresh = 0
)

dd <- case_study_data_trunc$delay_daily
dd[dd==0] <- 1e-3

hist(log(dd), freq=FALSE)
curve(dnorm(x, 1.47, exp(0.11)), add=T)

censor_trunc <- censoring_adjusted_delay(
  data = case_study_data_trunc, cores = 4, refresh = 0
)

censor_trunc_draws <- extract_lognormal_draws(censor_trunc)

trunc_trunc <- truncation_adjusted_delay(
  data = case_study_data_trunc, cores = 4, refresh = 0
)

trunc_trunc2 <- truncation_adjusted_delay(
  data = case_study_data_trunc2, cores = 4, refresh = 0
)

trunc_trunc_draws2 <- extract_lognormal_draws(naive_trunc2)

quantile(trunc_trunc_draws2$mean, c(0.025, 0.5, 0.975))
quantile(trunc_trunc_draws2$sd, c(0.025, 0.5, 0.975))

