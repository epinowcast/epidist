# Load packages
devtools::load_all() # for loading the local package functions
library(here) # for finding files
library(data.table) # for general data manipulation
library(arrow) # for loading data in arrow format
library(dplyr) # for manipulating arrow data
library(purrr) # for iterating over lists
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation
library(forcats) # manipulate factors
# Load case study data
case_study_obs <- fread(here("data/scenarios/ebola_case_study.csv"))

# Load available models
models <- fread(here("data/meta/models.csv"))

# Function to read each arrow dataset, filter for the case study
# and attach the model name
read_case_study <- function(target_model) {
  # Load posterior samples as an arrow dataset
  dataset <- open_dataset(
    here("data", "posteriors", models[model %in% target_model, in_code])
  )

  # Filter for the case study related samples
  # Add model name to each row
  samples <- dataset |>
    filter(data_type %in% "ebola_case_study") |>
    mutate(model = target_model) |>
    collect()

  return(samples)
}

# Load case study samples for each model and combine
cs_samples <- map_dfr(models$model, read_case_study)

# Get samples sizes
sample_sizes <- cs_samples[, .(sample_size, obs_type, scenario)] |>
    unique() |>
    DT(, sample_size := as.numeric(as.character(sample_size))) |>
    dcast(scenario  ~ obs_type, value.var = "sample_size") |>
    DT(, per := round(100 * `real-time` / retrospective, 1))
sample_sizes

# Get observation times
obs_times <- cs_samples |>
  DT(, unique(scenario)) |>
  gsub(" days", x = _, "") |>
  as.numeric() |>
  (\(x) x[order(x)])()

# Make inidividual plots

# Plot observed cases by observation window
truncated_cs_obs_by_window <- construct_cases_by_obs_window(
  case_study_obs, windows = obs_times, obs_type = "stime"
)

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window)

case_study_obs_summ_forward <- case_study_obs |>
  group_by(ptime_daily) |>
  summarize(
    mean=mean(delay_daily),
    lwr=ifelse(length(delay_daily) > 10, t.test(delay_daily)[[4]][1], NA),
    upr=ifelse(length(delay_daily) > 10, t.test(delay_daily)[[4]][2], NA)
  ) %>%
  mutate(
    type="Forward"
  )

case_study_obs_summ_retro <- case_study_obs |>
  group_by(ptime_daily) |>
  summarize(
    sum1=sum(delay_daily),
    sum2=sum(delay_daily^2),
    n=n()
  ) |>
  arrange(ptime_daily) |>
  mutate(
    ntotal=cumsum(n),
    mean=cumsum(sum1)/ntotal,
    sd=sqrt((cumsum(sum2)-2*cumsum(sum1)*mean+mean^2*ntotal)/ntotal),
    se=sd/sqrt(ntotal),
    lwr=mean-1.96*se,
    upr=mean+1.96*se
  )

case_study_obs_summ_realt <- case_study_obs |>
  group_by(stime_daily) |>
  summarize(
    sum1=sum(delay_daily),
    sum2=sum(delay_daily^2),
    n=n()
  ) |>
  arrange(stime_daily) |>
  mutate(
    ntotal=cumsum(n),
    mean=cumsum(sum1)/ntotal,
    sd=sqrt((cumsum(sum2)-2*cumsum(sum1)*mean+mean^2*ntotal)/ntotal),
    se=sd/sqrt(ntotal),
    lwr=mean-1.96*se,
    upr=mean+1.96*se
  )

tv_plot <- ggplot(case_study_obs) +
  geom_smooth(aes(ptime_daily, delay_daily, col="Forward", fill="Forward"), alpha=0.2) +
  geom_ribbon(data=case_study_obs_summ_retro, aes(ptime_daily, ymin=lwr, ymax=upr, fill="Retrospective"), alpha=0.2) +
  geom_line(data=case_study_obs_summ_retro, aes(ptime_daily, mean, col="Retrospective")) +
  geom_ribbon(data=case_study_obs_summ_realt, aes(stime_daily, ymin=lwr, ymax=upr, fill="Real-time"), alpha=0.2) +
  geom_line(data=case_study_obs_summ_realt, aes(stime_daily, mean, col="Real-time")) +
  geom_vline(xintercept=c(60, 120, 180, 240), lty=2, alpha=0.9) +
  scale_x_continuous("Days") +
  scale_y_continuous("Mean delay (days)") +
  scale_fill_brewer("Estimation method", palette = "Dark2") +
  scale_color_brewer("Estimation method", palette = "Dark2") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )


# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
truncated_cs_obs <- obs_times |>
  map_dfr(~ filter_obs_by_obs_time(case_study_obs, obs_time = .x)) |>
  combine_obs(case_study_obs) |>
  DT(, type := "Real-time")

retro_cs_obs <- obs_times |>
  map_dfr(~ filter_obs_by_ptime(case_study_obs, obs_time = .x)) |>
  combine_obs(case_study_obs) |>
  DT(, type := "Retrospective")

combined_cs_obs <- rbind(truncated_cs_obs, retro_cs_obs)

## couldn't figure out how to change the fill... sorry!
empirical_pmf_plot <- combined_cs_obs |>
  DT(delay_daily <= 20) |>
  DT(obs_at != 483) |>
  mutate(
    obs_at=factor(obs_at, levels=c("60", "120", "180", "240"))
  ) |>
  ggplot() +
  aes(x = delay_daily) +
  geom_histogram(
    aes(
      y = after_stat(density), fill = type), binwidth = 1,
    position = "dodge", col = "#696767b1", alpha=0.5
  ) +
  scale_fill_manual(values=palette.colors(3, "Dark2")[2:3]) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Days", y = "Density") +
  facet_wrap(vars(obs_at), nrow = 1)

case_study_obs_trunc_prop <- merge(
  case_study_obs %>%
    group_by(ptime_daily) %>%
    summarize(
      n_total=n()
    ) %>%
    mutate(
      n_total=cumsum(n_total)
    ) %>%
    rename(cohort=ptime_daily),
  case_study_obs %>%
    group_by(stime_daily) %>%
    summarize(
      n_obs=n()
    ) %>%
    mutate(
      n_obs=cumsum(n_obs)
    ) %>%
    rename(cohort=stime_daily)
) %>%
  arrange(cohort) %>%
  mutate(
    trunc=n_obs/n_total
  )

trunc_prop_plot <- ggplot(case_study_obs_trunc_prop) +
  geom_line(aes(cohort, trunc)) +
  geom_vline(xintercept=c(60, 120, 180, 240), lty=2, alpha=0.9) +
  scale_x_continuous("Days") +
  scale_y_continuous("Proportion observation truncated") +
  theme_bw()


# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- cs_samples |>
  draws_to_long() |>
  DT(value <= 10) |>
  DT(value >= -10) |>
  DT(parameter %in% c("mean", "sd")) |>
  DT(, scenario := gsub(" days", x = scenario, replacement = "") |>
    factor(levels = obs_times) |>
    fct_rev()
  ) |>
  DT(, obs_type := str_to_sentence(obs_type)) |>
  DT(, model := factor(model, levels = models$model))

# Plot posterior densities for each parameter by model and observation type.
# Filter out outlier values for the sake of plotting
parameter_density_plot <- clean_cs_samples |>
  draws_to_long() |>
  DT(value <= 10) |>
  DT(value >= -10) |>
  DT(parameter %in% c("mean", "sd")) |>
  DT(, parameter := str_to_sentence(parameter)) |>
  plot_recovery(y = scenario, fill = obs_type) +
  facet_grid(
    vars(model), vars(parameter),
    labeller = label_wrap_gen(multi_line = TRUE),
    scales = "free_x"
    ) +
  scale_fill_brewer(palette = "Dark2") +
  guides(
    fill = guide_legend(title = "Estimation method"), col = guide_none()
  ) +
  labs(
    y = "Observation day", x = "Parameter estimate"
  ) +
  theme(legend.position = "bottom")

case_study_plot1 <- obs_plot +
  tv_plot +
  empirical_pmf_plot +
  trunc_prop_plot +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides= "collect", nrow=4) &
  theme(legend.position = "bottom")

# Combine plots
case_study_plot2 <- parameter_density_plot +
plot_annotation(tag_levels = "A") +
theme(legend.position = "bottom")


# Save combined plots
ggsave(
  here("figures", "case_study1.png"), case_study_plot1,
  height = 12, width = 10, dpi = 330
)

# Save combined plots
ggsave(
  here("figures", "case_study2.png"), case_study_plot2,
  height = 12, width = 10, dpi = 330
)
