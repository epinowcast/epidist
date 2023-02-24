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
  dcast(scenario ~ obs_type, value.var = "sample_size") |>
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
  case_study_obs,
  windows = obs_times, obs_type = "stime"
)

obs_plot <- plot_cases_by_obs_window(truncated_cs_obs_by_window)

# Calculate forward and backward mean delay and plot
case_study_obs_summ_forward <- case_study_obs |>
  group_by(ptime_daily) |>
  summarize(mean = mean(delay_daily)) |>
  mutate(type = "Forward") |>
  rename(time = ptime_daily)

case_study_obs_summ_backward <- case_study_obs |>
  group_by(stime_daily) |>
  summarize(mean = mean(delay_daily)) |>
  mutate(type = "Backward") |>
  rename(time = stime_daily)

# Plot mean estimates
mean_plot <- list(case_study_obs_summ_forward, case_study_obs_summ_backward) |>
  rbindlist(fill = TRUE) |>
  DT(,
   type := factor(
    type, levels = c("Forward", "Backward"))
  ) |>
  ggplot() +
  aes(x = time, y = mean, col = type, fill = type) +
  geom_smooth(alpha = 0.4, method = "gam") +
  geom_vline(xintercept = c(60, 120, 180, 240), linetype = 2, alpha = 0.9) +
  scale_x_continuous("Days") +
  scale_y_continuous("Mean delay (days)") +
  scale_fill_brewer(
    "Mean type", palette = "Accent", aesthetics = c("fill", "colour")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# Plot empirical PMF for each observation window
# First construct observed and retrospective data by window and join with
# complete data
# Retrict to 20 days delay
obs_t_group <- c(0, 60, 120, 180, 240)

case_study_obs_retro <- case_study_obs |>
  copy() |>
  DT(, group := cut(ptime_daily, obs_t_group)) |>
  DT(!is.na(group)) |>
  DT(, type := "Retrospective")

## ensuring that both ptime and stime are observed between observation time
case_study_obs_realtime <- case_study_obs |>
  copy() |>
  DT(, group_s := cut(stime_daily, obs_t_group)) |>
  DT(, group_p := cut(ptime_daily, obs_t_group)) |>
  DT(group_s == group_p) |>
  DT(, group := group_p) |>
  dplyr::select(-group_s, -group_p) |>
  DT(, type := "Real-time")

combined_cs_obs <- rbind(case_study_obs_retro, case_study_obs_realtime)

combined_cs_obs_mean <- combined_cs_obs |>
  mutate(
    obs_at = factor(group, labels = c("60", "120", "180", "240"))
  ) |>
  group_by(obs_at, type) |>
  summarize(
    mean = mean(delay_daily)
  )

empirical_pmf_plot <- combined_cs_obs |>
  DT(delay_daily <= 15) |>
  mutate(
    obs_at = factor(group, labels = c("60", "120", "180", "240"))
  ) |>
  ggplot() +
  aes(x = delay_daily) +
  geom_vline(
    data = combined_cs_obs_mean, aes(xintercept = mean, col = type), lty = 2
  ) +
  geom_histogram(
    aes(
      y = after_stat(density), fill = type
    ),
    binwidth = 1, position = "dodge", alpha = 0.6, col = "#696767b1"
  ) +
  scale_fill_brewer("Estimation method", palette = "Dark2") +
  scale_color_brewer("Estimation method", palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Days", y = "Density") +
  facet_wrap(vars(obs_at), nrow = 1)

# Plot the proportion of secondary events that are truncated within a rolling
# 60 day observation window
# FIXME: THis is completely wrong as we need to know which secondary events
# are observed within the 60 day primary event window
# FIXME: This is also wrong as we need all days to be present and not just 
# those with events if attempting to filter backwards.
ptime_rolling_sum <- case_study_obs |>
  group_by(ptime_daily) |>
  summarize(
    n_total = n()
  ) |>
  full_join(
    tibble(
      ptime_daily = seq(
        min(case_study_obs$ptime_daily),
        max(case_study_obs$ptime_daily)
      )
    ),
     by = "ptime_daily"
  ) |>
  mutate(n_total = ifelse(is.na(n_total), 0, n_total)) |>
  arrange(ptime_daily) |>
  mutate(n_total = frollsum(n_total, 60, fill = NA)) |>
  rename(cohort = ptime_daily)

case_study_obs_trunc_prop <- ptime_rolling_sum |>
  mutate(obs = list(case_study_obs)) |>
  unnest(cols = c(obs)) |>
  filter(ptime_daily >= (cohort - 60)) |>
  filter(ptime_daily <= cohort) |>
  filter(stime_daily <= cohort) |>
  group_by(cohort) |>
  summarize(
    n_obs = n(),
    n_total = first(n_total)
  ) |>
  ungroup() |>
  mutate(
    trunc_prop = 1 - n_obs / n_total
  ) |>
  filter(!is.na(trunc_prop))

trunc_prop_plot <- ggplot(case_study_obs_trunc_prop) +
  geom_smooth(aes(x = cohort, y = trunc_prop), method = "gam", col = "black") +
  geom_vline(xintercept = c(60, 120, 180, 240), lty = 2, alpha = 0.9) +
  scale_x_continuous("Days", limits = c(0, NA)) +
  scale_y_continuous(
    str_wrap(
      "Unobserved secondary events from primary events in the past 60 days",
      width = 40
    ),
    labels = scales::percent
  ) +
  theme_bw()

# Clean posterior draws
clean_cs_samples <- cs_samples |>
  DT(, scenario := gsub(" days", x = scenario, replacement = "") |>
    factor(levels = obs_times) |>
    fct_rev()
  ) |>
  DT(, obs_type := str_to_sentence(obs_type)) |>
  DT(, model := factor(model, levels = models$model)) |>
  DT(,
    sample := 1:.N,
    keyby = c(
      "model", "scenario", "obs_type"
    )
  )

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
    y = "Observation day", x = "Days"
  ) +
  theme(legend.position = "bottom")

# Plot posterior predictions for each observation window of the cohort mean
truncated_draws <- clean_cs_samples |>
  DT(sample <= 1000) |> # use only 1000 samples for plotting
  DT(obs_type %in% "Real-time") |>
  DT(, obs_at := as.numeric(as.character(scenario))) |>
  DT(,
      {
        message(
          "Estimating truncated mean at: ", .SD[, unique(obs_at)], " days"
        )
        calculate_truncated_means(
        .SD,
        obs_at = .SD[, unique(obs_at)],
        ptime = c(.SD[, unique(obs_at)] - 60, .SD[, unique(obs_at)] - 1)
        )
      },
    by = "obs_at"
  ) |>
  summarise_variable(
    variable = "trunc_mean",
    by = c("obs_horizon", "model", "obs_at", "scenario")
  )

cohort_mean <- map_dfr(obs_times,
    ~ case_study_obs |>
      filter_obs_by_obs_time(obs_time = .) |>
      calculate_cohort_mean(
        type = "cohort",
        obs_at = .
      ) |>
      DT(, obs_at := .),
  .progress = TRUE
) |>
  DT(ptime_daily >= -60)


mean_pp <- truncated_draws |>
  plot_mean_posterior_pred(
    obs_mean = cohort_mean, col = model, fill = model,
    mean = TRUE, ribbon = TRUE
  ) +
  guides(
    fill = guide_legend(title = "Model", nrow = 4),
    col = guide_legend(title = "Model", nrow = 4)
  ) +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "colour")) +
  theme(legend.direction = "vertical") +
  facet_grid(vars(obs_at),
        labeller = label_wrap_gen(multi_line = TRUE),
  )

case_study_plot1 <- obs_plot +
  mean_plot +
  (empirical_pmf_plot + guides(color = guide_none())) +
  trunc_prop_plot +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 4) &
  theme(legend.position = "bottom")

# Combine plots
case_study_plot2 <- parameter_density_plot +
  mean_pp +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", widths = c(2, 1)) &
  theme(legend.position = "bottom")

# Save combined plots
ggsave(
  here("figures", "case_study1.pdf"), case_study_plot1,
  height = 16, width = 16, dpi = 330
)

# Save combined plots
ggsave(
  here("figures", "case_study2.pdf"), case_study_plot2,
  height = 16, width = 16, dpi = 330
)
