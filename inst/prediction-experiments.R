library(epidist)
library(tidyverse)

source("tests/testthat/setup.R")

data <- as_latent_individual(sim_obs)
fit <- epidist(data) 
pred <- predict_delay_parameters(fit, newdata = NULL)

pred |>
  group_by(index) |>
  summarise(
   mu_mean = mean(mu),
   sigma_mean = mean(sigma)
  ) |>
  ggplot(aes(x = index, y = mu_mean)) +
    geom_point() +
    theme_minimal()

pred1 <- predict_delay_parameters(fit, newdata = data[1, ])
pred2 <- predict_delay_parameters(fit, newdata = data[2, ])
plot(pred1$mu, pred2$mu)

pred12 <- predict_delay_parameters(fit, newdata = data[1:2, ])

pred12 |>
  group_by(index) |>
  summarise(mu = mean(mu))

newdata <- data[1, c("case", "delay_central", "obs_t", "pwindow_upr", "swindow_upr")]
newdata$swindow_upr <-  NA
newdata$pwindow_upr <- NA
newdata$obs_t <- NA
newdata$delay_central <- NA
newdata$case <- NA

# Putting all these things to NA doesn't do anything
pred1 <- predict_delay_parameters(fit, newdata)
pred1$mu |> mean()

set.seed(101)

obs_time <- 25
sample_size <- 500

meanlog_m <- 1.9
sdlog_m <- 0.3

meanlog_f <- 1.4
sdlog_f <- 0.7

sim_obs_sex <- simulate_gillespie()
sim_obs_sex$sex <- rbinom(n = nrow(sim_obs_sex), size = 1, prob = 0.5)

sim_obs_sex_m <- filter(sim_obs_sex, sex == 0) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog_m,
    sdlog = sdlog_m
  )

sim_obs_sex_f <- filter(sim_obs_sex, sex == 1) |>
  simulate_secondary(
    dist = rlnorm,
    meanlog = meanlog_f,
    sdlog = sdlog_f
  )

sim_obs_sex <- bind_rows(sim_obs_sex_m, sim_obs_sex_f) |>
  arrange(case)

sim_obs_sex <- sim_obs_sex |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = obs_time)

sim_obs_sex <- sim_obs_sex[sample(seq_len(.N), sample_size, replace = FALSE)]

ggplot(sim_obs_sex, aes(x = case, y = delay, col = as.factor(sex))) + 
  geom_point()
