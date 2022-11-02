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

truncated_obs <- obs |>
    filter_obs_by_obs_time(obs_time = 25)

model_path <- file.path(tempdir(), "model.stan")
model_code <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, fn = brms::make_stancode,
  save_model = model_path
)
compiled_model <- cmdstanr::cmdstan_model(model_path)

fitlist <- lapply(1:nrep, function(x) {
  message("Starting rep: ", x)
  set.seed(x)
  sample_obs <- truncated_obs |>
    DT(sample(1:.N, 200, replace = FALSE))

  data <- latent_truncation_censoring_adjusted_delay(
    data = sample_obs, fn = brms::make_standata
  )
  fit <- sample_model(
    model = model_path,
    data = data,
    diagnostics = TRUE,
    cores = 4,
    refresh = 0,
    show_messages = FALSE
  )
  return(fit[])
})

names(fitlist) <- paste0("fit", 1:20)

draws <- fitlist |>
  map(extract_lognormal_draws, from_dt = TRUE) |>
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
  DT(, .(mean = mean(value),
         lwr = quantile(value, 0.025),
         upr = quantile(value, 0.975)), by = c("model", "parameter"))


save(summ, file = "test_uniform.rda")
