prep_obs <- as_latent_individual(sim_obs)
prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)

fit_sex <- epidist(
  data = prep_obs,
  formula = brms::bf(mu ~ 1 + sex, sigma ~ 1 + sex)
)

draws <- posterior::as_draws_df(fit_sex$fit)

# With newdata = NULL
pred <- predict_delay_parameters(fit_sex)

# With newdata
strata_df <- prep_obs[1:2, ] |> as.data.frame()

strata_df <- strata_df |>
  dplyr::select(delay_central, sex, obs_t, pwindow_upr, swindow_upr)

pred_strata <- predict_delay_parameters(fit_sex, newdata = strata_df)

# extract_all_strata <- function() {
# 
# }

library(ggplot2)
ggplot(pred_strata, aes(y = mean)) +
  geom_histogram() +
  facet_grid(~ index) +
  coord_flip()  
