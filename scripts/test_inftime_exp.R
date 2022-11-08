library(data.table)
library(purrr, quietly = TRUE)
library(dplyr)
library(here)
library(ggplot2); theme_set(theme_bw())
library(egg)
library(dynamicaltruncation)

outbreak <- simulate_exponential_cases(sample_size = 10000, seed=101)

secondary_dist <- data.table(
  meanlog = 1.8, sdlog = 0.5
) |>
  add_natural_scale_mean_sd()

obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$meanlog[[1]],
    sdlog = secondary_dist$sdlog[[1]]
  ) |>
  observe_process()

set.seed(101)
truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 30) |>
  DT(sample(1:.N, 200, replace = FALSE))

latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 100
)

summ <- latent_truncation_censoring_fit |>
  posterior_summary()

ptime_est <- data.frame(
  id = factor(1:200),
  true = truncated_obs$ptime - truncated_obs$ptime_daily,
  est = summ[5:204, 1],
  lwr = summ[5:204, 3],
  upr= summ[5:204, 4]
)

stime_est <- data.frame(
  id = factor(1:200),
  true = truncated_obs$stime - truncated_obs$stime_daily,
  est = summ[205:404, 1],
  lwr = summ[205:404, 3],
  upr = summ[205:404, 4]
)

g1 <- ggplot(ptime_est) +
  geom_point(aes(true, id), col = "red") +
  geom_point(aes(est, id)) +
  geom_errorbar(aes(xmin = lwr, xmax = upr, y = id), width = 0) +
  scale_x_continuous("Estimated ptime") +
  theme(
    axis.text.y = element_blank()
  )

g2 <- ggplot(stime_est) +
  geom_point(aes(true, id), col = "red") +
  geom_point(aes(est, id)) +
  geom_errorbar(aes(xmin = lwr, xmax = upr, y = id), width = 0) +
  scale_x_continuous("Estimated stime") +
  theme(
    axis.text.y = element_blank()
  )

gcomb <- ggarrange(g1, g2, nrow = 1, draw = FALSE)

ggsave("test_inftime_exp.png", gcomb, width = 8, height = 6)

plot(stime_est$est, ptime_est$est)

plot(stime_est$est - ptime_est$est)

plot(stime_est$true - ptime_est$true)
