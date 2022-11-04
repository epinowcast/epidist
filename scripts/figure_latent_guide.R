library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)

epsilon <- 0.1

g1 <- ggplot(NULL) +
  geom_point(aes(0, 0), size=3, shape=21, fill="red", stroke=1) +
  geom_point(aes(1, 0), size=3, shape=21, fill="blue", stroke=1) +
  geom_segment(aes(0+epsilon, 0, xend=1-epsilon, yend=0), arrow = arrow(length = unit(0.2, "inches")), lwd=1) + 
  geom_point(aes(0, 0.1), size=3, shape=21, fill="red", stroke=1) +
  geom_point(aes(4, 0.1), size=3, shape=21, fill="blue", stroke=1, alpha=0.5) +
  geom_segment(aes(0+epsilon, 0.1, xend=4-epsilon, yend=0.1), arrow = arrow(length = unit(0.2, "inches")), lwd=1, lty=2) +
  geom_point(aes(1, 0.2), size=3, shape=21, fill="red", stroke=1) +
  geom_point(aes(2, 0.2), size=3, shape=21, fill="blue", stroke=1) +
  geom_segment(aes(1+epsilon, 0.2, xend=2-epsilon, yend=0.2), arrow = arrow(length = unit(0.2, "inches")), lwd=1) +
  geom_point(aes(4, 0.3), size=3, shape=21, fill="red", stroke=1, alpha=0.5) +
  geom_point(aes(5, 0.3), size=3, shape=21, fill="blue", stroke=1, alpha=0.5) +
  geom_segment(aes(4+epsilon, 0.3, xend=5-epsilon, yend=0.3), arrow = arrow(length = unit(0.2, "inches")), lwd=1, lty=2) +
  geom_segment(aes(3, -0.05, xend=3, yend=0.35), lty=1, col="gray50") +
  annotate("text", x=3, y=0.35, label="Observation time", vjust=-0.5) +
  annotate("text", x=4.5, y=-0.05, label="Future events", vjust=-1) +
  geom_rect(aes(xmin=3, ymin=-0.05, xmax=5.5, ymax=0.35), fill="gray", alpha=0.2) +
  scale_x_continuous("Day", limits=c(-0.2, 5.5), expand=c(0, 0),
                     breaks=0:5,
                     labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) +
  scale_y_continuous("Subject", breaks=c(0, 0.1, 0.2, 0.3),
                     labels=c(1, 2, 3, 4),
                     limits=c(-0.05, 0.38), expand=c(0, 0)) +
  ggtitle("A. Truncation") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

## imagine a case where number of positive tests are reported every Monday
## actual testing time is unknown
g2 <- ggplot(NULL) +
  geom_segment(aes(4, 0, xend=4, yend=0.5), lwd=1) + 
  geom_point(aes(4, 0.5), size=4, shape=21, fill="red", stroke=1) +
  annotate("text", x=4, y=0.5, label=c("Symptom onset"), vjust=-1) +
  geom_segment(aes(0+epsilon, 0.1, xend=4-epsilon, yend=0.1), arrow = arrow(length = unit(0.2, "inches")), lwd=1, lty=1) +
  geom_segment(aes(4-epsilon, 0.1, xend=0+epsilon, yend=0.1), arrow = arrow(length = unit(0.2, "inches")), lwd=1, lty=1) +
  annotate("text", x=2, y=0.1, label=c("Possible exposure dates"), vjust=-1) +
  scale_x_continuous("Day", limits=c(-0.2, 5.5), expand=c(0, 0),
                     breaks=0:5,
                     labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) +
  scale_y_continuous(limit=c(0, 2.5),
                     expand=c(0, 0)) +
  ggtitle("B. Censoring") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
  
tikz(file = "figure_latent_guide.tex", width = 10, height = 4, standAlone = T)
grid.arrange(g1, g2, nrow=1)
dev.off()
tools::texi2dvi('figure_latent_guide.tex', pdf = T, clean = T)
