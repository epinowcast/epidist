library(hexSticker)
library(sysfonts)
library(ggplot2)

# font setup
font_add_google("Zilla Slab Highlight", "useme")


# make standard plot
plot

# strip out most of the background
hex_plot <- plot +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none",
        panel.background = element_blank())

# make and save hexsticker
sticker(
  hex_plot,
  package = "epidist",
  p_size = 23,
  p_color = "#646770",
  s_x = 1,
  s_y = 0.85,
  s_width = 1.3,
  s_height = 0.85,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  url = "epidist.epinowcast.org",
  u_color = "#646770",
  u_size = 3.5
)
