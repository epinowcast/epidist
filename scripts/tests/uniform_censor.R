library(dynamicaltruncation)
library(data.table)
library(purrr)
library(ggplot2)
library(scoringutils)
library(egg)

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

## no truncation
truncated_obs <- obs |>
    filter_obs_by_obs_time(obs_time = max(obs$obs_at))

censor_delay <- calculate_censor_delay(truncated_obs)

## looks pretty uniform to me
plot_censor_delay(censor_delay)
g1 <- ggplot(truncated_obs) +
  geom_line(aes(ptime - ptime_daily, qunif(ptime - ptime_daily))) +
  theme_bw()

g2 <- ggplot(truncated_obs) +
  geom_line(aes(stime - stime_daily, qunif(stime - stime_daily))) +
  theme_bw()

egg::ggarrange(g1, g2, nrow=1)

latent_model_path <- file.path(tempdir(), "latent_model.stan")
latent_model_code <- latent_censoring_adjusted_delay(
  data = truncated_obs, fn = brms::make_stancode,
  save_model = latent_model_path
)
latent_compiled_model <- cmdstanr::cmdstan_model(latent_model_path)

censor_model_path <- file.path(tempdir(), "censor_model.stan")
censor_model_code <- censoring_adjusted_delay(
  data = truncated_obs, fn = brms::make_stancode,
  save_model = censor_model_path
)
censor_compiled_model <- cmdstanr::cmdstan_model(censor_model_path)

fitlist <- lapply(1:nrep, function(x) {
  message("Starting rep: ", x)
  set.seed(x)
  sample_obs <- truncated_obs |>
    DT(sample(1:.N, 200, replace = FALSE))

  latent_data <- latent_censoring_adjusted_delay(
    data = sample_obs, fn = brms::make_standata
  )
  
  censor_data <- censoring_adjusted_delay(
    data = sample_obs, fn = brms::make_standata
  )
  
  latent_fit <- sample_model(
    model = latent_model_path,
    data = latent_data,
    diagnostics = TRUE,
    parallel_chains = 4,
    refresh = 0,
    show_messages = FALSE
  )
  
  censor_fit <- sample_model(
    model = censor_model_path,
    data = censor_data,
    diagnostics = TRUE,
    parallel_chains = 4,
    refresh = 0,
    show_messages = FALSE
  )
  
  return(list(latent=latent_fit[], censor=censor_fit[]))
})

names(fitlist) <- paste0("fit", 1:nrep)

draws <- fitlist |>
  map(function(x) {
    x |> 
      map(extract_lognormal_draws, from_dt = TRUE) |>
      map(draws_to_long) |>
      rbindlist(idcol = "fit")
  }) |>
  rbindlist(idcol="model")

relative_draws <- draws |>
  make_relative_to_truth(secondary_dist)

scores <- relative_draws |>
  copy() |>
  DT(, sample := 1:.N, by = c("model", "fit", "parameter")) |>
  DT(, prediction := value) |>
  DT(, value := NULL) |>
  DT(, .(model, fit, parameter, prediction, true_value, sample)) |>
  score() |>
  summarise_scores(by = c("parameter", "fit")) |>
  summarise_scores(fun = signif, digits = 2)

scores

relative_draws |>
  plot_relative_recovery(fill=fit) +
  facet_wrap(vars(parameter), nrow = 1, scales = "free_x") +
  # guides(fill = guide_none()) +
  labs(
    y = "Fits", x = "Relative to ground truth"
  )

#summ <- draws |>
#  DT(, .(mean = mean(value),
#         lwr = quantile(value, 0.025),
#         upr = quantile(value, 0.975)), by = c("model", "parameter")
#  )

# save(summ, file = "test_uniform.rda")

summ <- fitlist |>
  map(function(x) {
    lf <- x[[1]]
    
    dd <- lf$fit[[1]]
    
    ss <- dd$summary()
    
    data.frame(
      pwindow=ss[4:203,2],
      swindow=ss[204:403,2],
      id=1:200
    )
  }) |>
  rbindlist(idcol="fit")

ggplot(summ) +
  geom_point(aes(mean, mean.1)) +
  labs(x="ptime", y="stime") +
  facet_wrap(~fit)

