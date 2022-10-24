library(brms)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
source("param.R")
source("R/apply_dynamic_correction.R")
load("rdacache/fit_exponential.rda")
load("rdacache/fit_exponential_doublecensor.rda")

r <- seq(-0.2, 0.2, length.out=11)
truemean <- exp(logmean + logsd^2/2)

ff_rtrunc <- rtrunc_model %>%
  apply_dynamic_correction(r=r, subsample=4000) %>%
  mutate(
    fit="Exact time known + right truncation"
  )

fit_exponential_doublecensor2 <- fit_exponential_doublecensor %>%
  mutate(
    fit="Doubly censored time + right truncation"
  )

ff_all <- bind_rows(
  ff_rtrunc,
  fit_exponential_doublecensor2
)

g1 <- ggplot(ff_all) +
  geom_point(aes(r, estimate, col=fit, shape=fit), position=position_dodge(0.015)) +
  geom_errorbar(aes(r, ymin=lwr, ymax=upr, col=fit), width=0, position=position_dodge(0.015)) +
  geom_hline(yintercept=truemean, lty=2) +
  scale_y_log10() +
  scale_color_viridis_d(end=0.8) +
  theme(
    legend.position = "top"
  )

ggsave("figure_doublecensor.pdf", g1, width=10, height=8)
ggsave("figure_doublecensor.png", g1, width=10, height=8)

