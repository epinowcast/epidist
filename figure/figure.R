library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)

summarised_posteriors_folder <- "../data/summarised_posteriors/"

allfiles <- list.files(summarised_posteriors_folder)

allsumm <- lapply(allfiles, function(x) {
  rr <- read.csv(paste0(summarised_posteriors_folder, x))
  
  rr$method <- gsub("\\.csv", "", x)
  
  rr
}) %>%
  bind_rows

g1 <- ggplot(filter(allsumm, parameter=="meanlog")) +
  geom_point(aes(mean, method)) +
  geom_errorbarh(aes(xmin=q2.5, xmax=q97.5, y=method), height=0) +
  geom_vline(xintercept=1.6, lty=2) +
  scale_x_continuous("meanlog") +
  ggtitle("meanlog") +
  facet_wrap(~scenario)
  
g2 <- ggplot(filter(allsumm, parameter=="sdlog")) +
  geom_point(aes(mean, method)) +
  geom_errorbarh(aes(xmin=q2.5, xmax=q97.5, y=method), height=0) +
  geom_vline(xintercept=0.6, lty=2) +
  scale_x_continuous("sdlog") +
  ggtitle("sdlog") +
  facet_wrap(~scenario)

gcomb <- ggarrange(g1, g2, nrow=2, draw=FALSE)

ggsave("figure.png", gcomb, width=8, height=6)
