library(dynamicaltruncation)
library(data.table)
library(purrr)
library(ggplot2)

nrep <- 20

outbreak <- simulate_exponential_cases(r=0)

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

fitlist <- lapply(1:nrep, function(x) {
  set.seed(x)
  truncated_obs <- obs |>
    filter_obs_by_obs_time(obs_time = 25) |>
    DT(sample(1:.N, 200, replace = FALSE))
  
  latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
    data = truncated_obs, cores = 4, refresh = 0
  )
  
  latent_truncation_censoring_fit
}) 

names(fitlist) <- paste0("fit", 1:20)

draws <- fitlist |>
  map(extract_lognormal_draws) |>
  map(draws_to_long) |>
  rbindlist(idcol = "model")

draws |>
  make_relative_to_truth(secondary_dist) |>
  plot_relative_recovery(fill = model) +
  facet_wrap(vars(parameter), nrow = 1, scales = "free_x") +
  scale_fill_viridis_d() +
  guides(fill = guide_none()) +
  labs(
    y = "Model", x = "Relative to ground truth"
  )

summ <- draws |>
  DT(, .(mean=mean(value),
         lwr=quantile(value, 0.025),
         upr=quantile(value, 0.975)), by=c("model", "parameter"))


save(summ, file="test_uniform.rda")
