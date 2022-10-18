library(brms)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
source("param.R")
source("correct.R")
load("fit_exponential.rda")

r <- seq(-0.2, 0.2, length.out=11)
truemean <- exp(logmean + logsd^2/2)

organize <- function(x) {
  x <- as.data.frame(x)
  x$names <- rownames(x)
  
  x %>%
    filter(!grepl("sigma", names)) %>%
    mutate(
      names=gsub("as\\.factorr", "", names),
      names=gsub("M", "-", names),
      r=as.numeric(names)
    )
}

## not subsampling despite its name...

ff_naive <- naive_model %>%
   dcorrect(r=r, subsample=4000) %>%
  mutate(
    fit="Naive"
  )

ff_dynamic <- naive_model %>%
  dcorrect(r=r, subsample=4000, correct=TRUE) %>%
  mutate(
    fit="Dynamical correction"
  )

ff_rtrunc <- rtrunc_model %>%
  dcorrect(r=r, subsample=4000) %>%
  mutate(
    fit="Right truncation"
  )

ff_rtrunc_dynamic <- rtrunc_model %>%
  dcorrect(r=r, subsample=4000, correct=TRUE) %>%
  mutate(
    fit="Right truncation + dynamical correction"
  )

ff_ltrunc_dynamic <- ltrunc_model %>%
  dcorrect(r=r, subsample=4000, correct=TRUE) %>%
  mutate(
    fit="Left truncation + dynamical correction"
  )

ff_all <- bind_rows(
  ff_naive,
  ff_dynamic,
  ff_rtrunc,
  ff_rtrunc_dynamic,
  ff_ltrunc_dynamic
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

ggsave("figure_exponential.pdf", g1, width=10, height=8)
ggsave("figure_exponential.png", g1, width=10, height=8)
