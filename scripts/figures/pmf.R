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
samples <- 100000

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
        pmf = rboundedgrowth(samples, x, 0, 1)
      )
    )
) |>
  rbindlist(idcol = "growth_rate")

# Define PMFs when the primary censoring interval is defined
# by the growth rate
growth_rate_pmfs <- growth_rates |>
  map(
    \(r) (
      simulate_double_censored_pmf_dt(
        rprimary = \(x) (rboundedgrowth(x, r, 0, 1)),
        rsecondary = \(x) (runif(x, 0, 1))
      ) |>
        DT(, growth_rate := r)
    )
  ) |>
  rbindlist(idcol = "growth_rate")

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
  ),
  # PMF using no prior for either event
  no_prior = data.table(
    sample = 1:samples,
    value = 0
  )
), idcol = "method")

#  Simulate PMFs for each approach
approximiate_pmfs <- rbindlist(list(
  # PMF using uniform prior for both events
  double_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (runif(x, 0, 1)),
    rsecondary = \(x) (runif(x, 0, 1))
  ),
  # PMF using a uniform prior on the delay rather than on the event of two days.
  two_day_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    rsecondary = \(x) (runif(x, -1, 1))
  ),
  # PMF using no prior for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    rsecondary = \(x) (runif(x, 0, 1))
  ),
  # PMF using a 0.5 shift for the primary event and a uniform prior for the
  # secondary event
  one_day_uniform_with_shift = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0.5),
    rsecondary = \(x) (runif(x, 0, 1))
  ),
  # PMF using no prior for either event
  no_prior = simulate_double_censored_pmf_dt(
    rprimary = \(x) (0),
    rsecondary = \(x) (0)
  )
), idcol = "method")

# Plot the PMF for each method
simulated_pmfs |>
  ggplot() +
  aes(x = delay, y = pmf) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  stat_function(
    fun = dlnorm, args = c(meanlog, sdlog), n = 100,
    col = "#000000b1"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(method))

# Plot the prior for each method
prior_samples |>
  ggplot() +
  aes(x = value, col = method) +
  geom_density() +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_none(),
    col = guide_legend(title = "Method", nrow = 2)
  )

# Summarise the mean and standard deviation of the PMF for each method
pmf_mean_and_sd <- simulated_pmfs |>
  DT(, .(
    mean = round(sum(delay * pmf), 2),
    sd = round(
      sqrt(sum(delay^2 * pmf) - sum(delay * pmf)^2),
      2
    )
  ), by = "method"
)

# Plot the mean and standard deviation of the PMF for each method
pmf_mean_and_sd |>
  ggplot() +
  aes(x = mean, y = sd, col = method, shape = method) +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Mean", y = "Standard deviation") +
  guides(
    col = guide_legend(title = "Method", nrow = 2),
    shape = guide_legend(title = "Method", nrow = 2)
  )

# Plot:
# Primary event censoring interval for each method
# Primary event censoring interval under different growth rates
# Realised censoring interval for each method
# Realised censoring interval under a range of growth rate assumptions
# PMF for each approximation vs growth rate pmfs (super imposed as points)
# Emipirical mean and standard deviation of the PMF for each method using colour