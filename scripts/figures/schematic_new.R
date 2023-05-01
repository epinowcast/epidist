devtools::load_all()
library(here)
library(dplyr)
library(ggplot2)
library(patchwork)

size <- 16

outbreak <- simulate_exponential_cases(sample_size=10000, seed=123, t=30) |>
  arrange(ptime) |>
  mutate(
    case=1:10000
  )

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

g1 <- ggplot(obs) +
  geom_point(aes(ptime, case), size=1, shape=1, col="orange") +
  geom_point(aes(stime, case), size=0.1, shape=2, col="purple") + 
  geom_vline(xintercept=25, lty=2, lwd=1) +
  annotate("text", x=15, y=9800, label="Reference time") +
  ggtitle("A. Empirical density") +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(0, 60)) +
  scale_y_continuous("id", expand=c(0, 0)) +
  theme_bw(base_size = size)

obs_forward <- obs |>
  filter(ptime_daily==25)

set.seed(101)
obs_forward_sample <- obs_forward |>
  DT(sample(1:.N, size=10))

forward <- ggplot(obs_forward_sample) +
  geom_segment(aes(ptime, as.factor(case), xend=stime, yend=as.factor(case)), lwd=1) +
  geom_point(aes(ptime, as.factor(case)), size=4, shape=21, fill="orange") +
  geom_point(aes(stime, as.factor(case)), size=4, shape=24, fill="purple") + 
  ggtitle("B. Forward") +
  scale_x_continuous("Time (days)") +
  scale_y_discrete("id") +
  theme_bw(base_size = size)

forward_delay <- ggplot(obs_forward) +
  geom_density(aes(delay), fill="orange", alpha=0.3) +
  geom_function(fun=function(x) dlnorm(x, 1.8, 0.5), lwd=1, n=201) +
  scale_x_continuous("Delay (days)", limits=c(0, 35)) +
  scale_y_continuous("Density") +
  theme_bw(base_size = size)

obs_backward <- obs |>
  filter(stime_daily==25)

set.seed(101)
obs_backward_sample <- obs_backward |>
  DT(sample(1:.N, size=10))

backward <- ggplot(obs_backward_sample) +
  geom_segment(aes(ptime, as.factor(case), xend=stime, yend=as.factor(case)), lwd=1) +
  geom_point(aes(ptime, as.factor(case)), size=4, shape=21, fill="orange") +
  geom_point(aes(stime, as.factor(case)), size=4, shape=24, fill="purple") + 
  ggtitle("C. Backward") +
  scale_x_continuous("Time (days)") +
  scale_y_discrete("id") +
  theme_bw(base_size = size)

backward_delay <- ggplot(obs_backward) +
  geom_density(aes(delay), fill="purple", alpha=0.3) +
  geom_function(fun=function(x) dlnorm(x, 1.8, 0.5), lwd=1, n=201) +
  scale_x_continuous("Delay (days)", limits=c(0, 35)) +
  scale_y_continuous("Density") +
  theme_bw(base_size = size)

obs_truncation <- obs |>
  filter(ptime < 25) 

set.seed(107)
obs_truncation_sample <- obs_truncation |>
  DT(sample(1:.N, size=10)) |>
  mutate(
    type = stime < 25
  )

truncation <- ggplot(obs_truncation_sample) +
  geom_segment(aes(ptime, as.factor(case), xend=stime, yend=as.factor(case), lty=type), lwd=1) +
  geom_point(aes(ptime, as.factor(case)), size=4, shape=21, fill="orange") +
  geom_point(aes(stime, as.factor(case), fill=type), size=4, shape=24) + 
  geom_vline(xintercept=25, lty=2) +
  ggtitle("D. Truncation") +
  scale_x_continuous("Time (days)", limits=c(0, 35)) +
  scale_y_discrete("id") +
  scale_linetype_manual(values=c(3, 1)) +
  scale_fill_manual(values=c("#EEBFF2", "purple")) +
  theme_bw(base_size = size) +
  theme(
    legend.position = "none"
  )

truncation_delay <- ggplot(filter(obs_truncation, stime < 25)) +
  geom_density(aes(delay), fill="orange", alpha=0.3) +
  geom_function(fun=function(x) dlnorm(x, 1.8, 0.5), lwd=1, n=201) +
  scale_x_continuous("Ddelay (days)", limits=c(0, 35)) +
  scale_y_continuous("Density") +
  theme_bw(base_size = size)

censor <- ggplot(obs_truncation_sample) +
  geom_point(aes(ptime_daily, as.factor(case)), size=4, shape=21, fill="orange") +
  geom_point(aes(ptime, as.factor(case)), size=4, shape=21) +
  geom_point(aes(stime_daily, as.factor(case)), size=4, shape=24, fill="purple") + 
  geom_point(aes(stime, as.factor(case)), size=4, shape=24) +
  ggtitle("E. Daily censoring") +
  scale_x_continuous("Time (days)", limits=c(0, 35)) +
  scale_y_discrete("id") +
  theme_bw(base_size = size) +
  theme(
    legend.position = "none"
  )

censor_delay <- ggplot(obs_truncation) +
  geom_bar(aes(x=delay_daily, y=(..count..)/sum(..count..)), fill="orange", alpha=0.3, col="black") +
  geom_function(fun=function(x) dlnorm(x, 1.8, 0.5), lwd=1, n=201) +
  scale_x_continuous("Delay (days)", limits=c(0, 35)) +
  scale_y_continuous("Density") +
  theme_bw(base_size = size)

p <- g1 + (forward + forward_delay +
  backward + backward_delay +
  truncation + truncation_delay +
  censor + censor_delay +
  plot_layout(nrow=2)) +
  plot_layout(nrow=1, widths=c(0.6, 2))

ggsave("figures/schematic_new.pdf", p, width = 20, height = 8)
