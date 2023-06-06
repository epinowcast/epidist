devtools::load_all()
library(data.table) # for general data manipulation
library(purrr) # for functional programming
library(here) # for finding files
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation

# Define a continuous PMF (could consider more than one delay but not likely
# we want to)
# Define a range of growth rates
# Show the implied prior for the primary event for each growth rate alongside
# the various discrete approximations priors (or impled priors).
# Show the PMF for each growth rate alongside the various discrete approximations.
# Show the mean and standard deviation of the PMF for each growth rate and the
# various discrete approximations.
growth_rates <- fread(here("data/meta/growth_rates.csv")) |>
  DT(, growth_rate := paste0(
    str_to_sentence(scenario),
    " (", round(r, 1), ")"
  )) |>
  DT(, growth_rate := factor(growth_rate))

# Load distributions
distributions <- fread(here("data/meta/distributions.csv"))
meanlog <- distributions[scenario %in% "medium", meanlog]
sdlog <- distributions[scenario %in% "medium", sdlog]

# Number of samples
samples <- 1e6

# Maximum delay
max_delay <- 20

# Write a helper function to return the PMF as a data.table
simulate_double_censored_pmf_dt <- function(
  alpha = meanlog, beta = sdlog, n = samples, max = max_delay,
  rdelay = rlnorm, ...
) {
  data.table(
    delay = 0:max_delay,
    pmf = simulate_double_censored_pmf(
      alpha = alpha, beta = beta, max = max_delay, n = samples,
      rdelay = rdelay, ...
    )
  )[]
}

# Define the function to generate random samples
rboundedgrowth <- function(n, r, p_L, p_R) {
  # Generate uniform random numbers
  u <- runif(n)

  # Use the inverse transform sampling method
  # P(x| P_L, P_R) = r * exp(rx) / (exp(r * P_R) - exp(r * P_L)) # nolint
  if (r != 0) {
    samples <- log(u * (exp(r * p_R) - exp(r * p_L)) + exp(r * p_L)) / r
  } else {
    samples <- u * (p_R - p_L) + p_L
  }
  return(samples)
}

growth_rates <- c(-2, -0.2, 0, 0.2, 2)
names(growth_rates) <- growth_rates

# Define growth rate samples
growth_rate_primary_samples <- growth_rates |>
  map(
    \(x) (
        data.table(
        sample = 1:samples,
        primary_value = rboundedgrowth(samples, x, 0, 1)
      ) |>
      DT(,
        value := runif(samples, 0, 1) - primary_value
      )
    )
) |>
  rbindlist(idcol = "growth_rate") |>
  DT(, growth_rate := factor(growth_rate, levels = names(growth_rates)))

# Define PMFs when the primary censoring interval is defined
# by the growth rate
growth_rate_pmfs <- growth_rates |>
  map(
    \(r) (
      simulate_double_censored_pmf_dt(
        rprimary = \(x) (rboundedgrowth(x, r, 0, 1))
      )
    )
  ) |>
  rbindlist(idcol = "growth_rate") |>
  DT(, growth_rate := factor(growth_rate, levels = names(growth_rates)))

# Define a prior on the delay censoring for each approximate approach
approximate_primary_samples <- rbindlist(list(
  # PMF using uniform prior for both events
  double_uniform = data.table(
    sample = 1:samples,
    value = (\(x) (runif(x, 0, 1) - runif(x, 0, 1)))(samples)
  ),
  # PMF using a uniform prior on the delay rather than on the event of two days.
  two_day_uniform = data.table(
    sample = 1:samples,
    value = (\(x) (runif(x, -1, 1)))(samples)
  ),
  # PMF using no prior for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform = data.table(
    sample = 1:samples,
    value = (\(x) (runif(x, 0, 1)))(samples)
  ),
  # PMF using a 0.5 shift for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform_with_shift = data.table(
    sample = 1:samples,
    value = (\(x) (runif(x, 0, 1) - 0.5))(samples)
  )
), idcol = "method")

#  Simulate PMFs for each approach
approximate_pmfs <- rbindlist(list(
  # PMF using uniform prior for both events
  double_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (runif(x, 0, 1))
  ),
  # PMF using a uniform prior on the delay rather than on the event of two days.
  two_day_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    delay_obs_process = \(s, p) (s - p + runif(length(s), -1, 1))
  ),
  # PMF using no prior for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    delay_obs_process = \(s, p) (s - p + runif(length(s), 0, 1))
  ),
  # PMF using a 0.5 shift for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform_with_shift = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    delay_obs_process = \(s, p) (s - p + runif(length(s), 0, 1) - 0.5)
  ),
  # PMF using no prior for either event
  no_prior = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    delay_obs_process = \(s, p) (s - p)
  )
), idcol = "method")

# Define meaningful names for the methods
methods <- data.table(
  method = c(
    "double_uniform",
    "two_day_uniform",
    "one_day_uniform",
    "one_day_uniform_with_shift",
    "no_prior"
  ),
  method_name = c(
    "Uniform(0, 1) interval for each event",
    "Uniform(-1, 1) interval across both events",
    "Uniform(0, 1) interval across both events", # nolint
    "Assume primary event at 0.5 and a Uniform(0, 1) interval for the secondary event", # nolint
    "No interval for either event"
  )
)

methods[, method_name := factor(method_name, levels = method_name)]

# Merge the methods with the sampled censoring intervals
approximate_primary_samples <- approximate_primary_samples |>
  merge(methods, by = "method")

# Merge the methods with the approximate PMFs
approximate_pmfs <- approximate_pmfs |>
  merge(methods, by = "method")


# Plot censoring interval for a range of methods
plot_censoring_interval <- function(data, vintercept = 0, ...) {
  data |>
    ggplot() +
    aes(...) +
    geom_density(linewidth = 1.2) +
    geom_vline(
      xintercept = vintercept, linetype = "dashed"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_none()
    ) +
    labs(
      x = "Censoring interval (days)",
      y = "Density"
    )
}

# Plot the primary censoring interval for each growth rat
growth_rate_primary_plot <- growth_rate_primary_samples |>
  copy() |>
  DT(, value := primary_value) |>
  plot_censoring_interval(x = value, col = growth_rate, vintercept = 0.5) +
  guides(col = guide_legend(title = "Growth rate", nrow = 2)) +
  scale_colour_viridis_d(option = "C")

# Plot the censoring interval for each growth rate
growth_rate_censoring_plot <- growth_rate_primary_samples |>
  plot_censoring_interval(x = value, col = growth_rate, ) +
  guides(col = guide_legend(title = "Growth rate", nrow = 2)) +
  scale_colour_viridis_d(option = "C")

# Plot the censoring interval for each method
approximate_censoring_plot <- approximate_primary_samples |>
  plot_censoring_interval(x = value, col = method_name) +
  guides(col = guide_legend(title = "Method", nrow = 2)) +
  scale_colour_brewer(palette = "Dark2")

# Plot the PMF for each method
approximate_pmfs_plot <- approximate_pmfs |>
  ggplot() +
  aes(x = delay, y = pmf) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.4) +
  stat_function(
    fun = dlnorm, args = c(meanlog, sdlog), n = 100,
    col = "#000000b1"
  ) +
  theme_bw() +
  geom_point(
    data = growth_rate_pmfs,
    aes(col = growth_rate),
  ) +
  labs(x = "Delay (days)", y = "Probability") +
  theme(legend.position = "bottom") +
  facet_wrap(
    vars(method_name), nrow = 1, labeller = label_wrap_gen(multi_line = TRUE)
  ) +
  guides(col = guide_legend(title = "Growth rate", nrow = 2))  +
  scale_colour_viridis_d(option = "C")

# Summarise the mean and standard deviation of the PMF for each method
calc_empirical_summary_stat <- function(data, by) {
  data |>
    DT(, .(
      mean = round(sum(pmf * delay), 2),
      sd = round(sqrt(sum(((delay - sum(pmf * delay))^2) * pmf)), 2)
    ), by = by)
}

approximate_pmf_mean_and_sd <- approximate_pmfs |>
  calc_empirical_summary_stat(by = "method_name")

growth_rate_pmf_mean_and_sd <- growth_rate_pmfs |>
  calc_empirical_summary_stat(by = "growth_rate")

# Plot the mean and standard deviation of the PMF for each method
pmf_mean_and_sd_plot <- approximate_pmf_mean_and_sd  |>
  ggplot() +
  aes(x = mean, y = sd, shape = method_name) +
  geom_point(size = 5, col = "#6d6d6d") +
  geom_point(
    data = growth_rate_pmf_mean_and_sd,
    aes(col = growth_rate), shape = 5, size = 5
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Mean", y = "Standard deviation") +
  guides(
    col = guide_legend(title = "Growth rate", nrow = 2),
    shape = guide_legend(title = "Method", nrow = 3)
  ) +
  scale_colour_viridis_d(option = "C")

# Plot
# Primary event censoring interval under different growth rates
# Realised censoring interval for each method
# Realised censoring interval under a range of growth rate assumptions
# PMF for each approximation vs growth rate pmfs (super imposed as points)
# Emipirical mean and standard deviation of the PMF for each method using colour
censoring_interval_plot <- (
  (
    (
      (
        (
          growth_rate_primary_plot |
          growth_rate_censoring_plot + guides(col = guide_none())
        ) +
        plot_layout(widths = c(1, 1), guides = "collect", nrow = 1)
      ) |
      approximate_censoring_plot
    ) +
    plot_layout(widths = c(1, 1, 1), guides = "collect", nrow = 1)
  ) /
    (approximate_pmfs_plot + guides(col = guide_none())) /
    (pmf_mean_and_sd_plot)
) +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(2, 2, 2), guides = "collect", nrow = 3) &
  theme(legend.position = "bottom")

ggsave(
  here("figures", "censoring-intervals.pdf"), censoring_interval_plot,
  height = 12, width = 12, dpi = 330
)
