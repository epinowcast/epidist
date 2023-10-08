devtools::load_all()
library(data.table) # for general data manipulation
library(purrr) # for functional programming
library(here) # for finding files
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation
library(dplyr)

# Load distributions
distributions <- fread(here("data/meta/distributions.csv"))
meanlog <- distributions[scenario %in% "medium", meanlog]
sdlog <- distributions[scenario %in% "medium", sdlog]

# Number of samples
samples <- 1e5

# Maximum delay
max_delay <- 20

rvec <- c(-2, -0.2, 0, 0.2, 2)

glist <- vector('list', length(rvec))
for (i in 1:length(rvec)) {
  r <- rvec[i]
  
  
  ss <- simulate_exponential_cases(r=r,
                                   sample_size = samples) |>
    simulate_secondary(
      meanlog = meanlog,
      sdlog = sdlog
    ) 
  
  ss_obs <- observe_process(ss)
  
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
  
  approximate_primary_summ <- split(approximate_primary_samples, approximate_primary_samples$method) |>
    lapply(function(x) {
      x$delay <- ss_obs$delay_daily + x$value
      x
    }) %>%
    bind_rows |>
    group_by(method) |>
    summarize(
      mean=mean(delay),
      sd=sd(delay)
    )
  
  g1 <- ggplot(ss_obs) +
    geom_density(aes((ptime-ptime_daily))) +
    theme_bw() +
    scale_x_continuous("Primary event time (days)") +
    scale_y_continuous("Density")
  
  g2 <- ggplot(approximate_primary_samples) +
    geom_density(data=ss_obs, aes((stime-stime_daily)-(ptime-ptime_daily)), lwd=1) +
    geom_density(aes(value, col=method)) +
    theme_bw() +
    scale_x_continuous("Censoring interval (days)") +
    scale_y_continuous("Density")
  
  g3 <- ggplot(approximate_primary_summ) +
    geom_point(aes(mean, sd, col=method, shape=method), size=2) +
    geom_hline(yintercept=distributions[scenario %in% "medium", sd]) +
    geom_vline(xintercept=distributions[scenario %in% "medium", mean]) +
    scale_x_continuous("Mean", limits=c(5.3, 6.8))  +
    scale_y_continuous("Sd", limits=c(3.8, 4.1)) +
    theme_bw()
  
  glist[[i]] <- g1 + g2 + g3+
    plot_layout(guides = "collect", nrow=3) &
    theme(legend.position = "bottom")
}

censoring_interval_new_plot <- ((((glist[[1]])/(glist[[2]]))/(glist[[3]]))/glist[[4]])/glist[[5]] +
  plot_annotation(tag_levels="A") +
  plot_layout(guides = "collect", ncol=5)

ggsave(
  here("figures", "censoring-intervals-new.pdf"), censoring_interval_new_plot,
  height = 12, width = 15, dpi = 330
)
