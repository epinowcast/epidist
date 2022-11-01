library(data.table)
library(purrr, quietly = TRUE)
library(here)
library(ggplot2); theme_set(theme_bw())
library(egg)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)

rvec <- c(-0.2, 0, 0.2)
meanlog <- 1.6
sdlog <- 0.5

nsim <- 20

set.seed(101)
out <- lapply(rvec, function(r) {
  sim <- lapply(1:nsim, function(x) {
    outbreak <- simulate_exponential_cases(r=r)
    
    obs <- outbreak |>
      simulate_secondary(
        meanlog = 1.8,
        sdlog = 0.5
      ) |>
      observe_process() |>
      DT(delay_daily > 0)
    
    data.table(
      meanlog_cont=mean(log(obs$delay)),
      sdlog_cont=sd(log(obs$delay)),
      meanlog_dis=mean(log(obs$delay_daily)),
      sdlog_dis=sd(log(obs$delay_daily)),
      sim=x,
      r=r
    )
  }) |>
    rbindlist()
}) |>
  rbindlist()

g1 <- ggplot(out) +
  geom_point(aes(sdlog_cont, sdlog_dis)) +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Continuous sdlog") +
  scale_y_continuous("Discrete sdlog") +
  facet_wrap(~r)

g2 <- ggplot(out) +
  geom_boxplot(aes(r, y=sdlog_dis/sdlog_cont, group=r)) +
  scale_y_continuous("Ratio bewteen discrete and continuous sdlog estimates")

gtot <- ggarrange(g1, g2, nrow=1, draw=FALSE, widths=c(3, 1))

ggsave("test_discrete.png", gtot, width=10, height=4)

g3 <- ggplot(out) +
  geom_point(aes(meanlog_cont, meanlog_dis)) +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Continuous meanlog") +
  scale_y_continuous("Discrete meanlog") +
  facet_wrap(~r)

g4 <- ggplot(out) +
  geom_boxplot(aes(r, y=meanlog_dis/meanlog_cont, group=r)) +
  scale_y_continuous("Ratio bewteen discrete and continuous meanlog estimates")

gtot2 <- ggarrange(g3, g4, nrow=1, draw=FALSE, widths=c(3, 1))

ggsave("test_discrete2.png", gtot2, width=10, height=4)
