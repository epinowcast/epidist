library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())

summarised_posteriors_folder <- "../data/summarised_posteriors/"

allfiles <- list.files(summarised_posteriors_folder)

allsumm <- lapply(allfiles, function(x) {
  rr <- read.csv(paste0(summarised_posteriors_folder, x))
  
  rr$method <- gsub("\\.csv", "", x)
  
  rr
}) %>%
  bind_rows

g1 <- ggplot(filter(allsumm, parameter=="mean")) +
  geom_point(aes(mean, method)) +
  geom_errorbarh(aes(xmin=q2.5, xmax=q97.5, y=method), height=0) +
  geom_vline(xintercept=exp(1.2 + 0.4^2/2), lty=2) +
  facet_wrap(~scenario)

ggsave("figure.pdf", g1, width=8, height=6)
ggsave("figure.png", g1, width=8, height=6)
