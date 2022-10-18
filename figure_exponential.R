library(brms)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
source("param.R")
load("fit_exponential.rda")

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

ff_naive <- fixef(naive_model) %>%
  organize %>%
  mutate(
    fit="Naive"
  )

ff_rtrunc <- fixef(rtrunc_model) %>%
  organize %>%
  mutate(
    fit="Right truncation"
  )

ff_all <- bind_rows(
  ff_naive,
  ff_rtrunc
)

g1 <- ggplot(ff_all) +
  geom_point(aes(r, Estimate, col=fit, shape=fit), position=position_dodge(0.01)) +
  geom_errorbar(aes(r, ymin=Q2.5, ymax=Q97.5, col=fit), width=0, position=position_dodge(0.01)) +
  scale_color_viridis_d(end=0.8)

ggsave("figure_exponential.pdf", g1, width=6, height=4)
ggsave("figure_exponential.png", g1, width=6, height=4)
