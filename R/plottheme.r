
library(extrafont)
# Standard plotting theme
plottheme = function() {
  theme(
    text = element_text(family = "Calibri", size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.key = element_blank(),
    legend.title = element_text(family = "Calibri", size = 10, face = "bold"),
    legend.text = element_text(family = "Calibri", size = 10),
    plot.title = element_text(size = 11, face = "bold"),
    axis.line = element_line(colour = "grey"),
    panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
    panel.grid.major.x = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "gray", fill = NA),
    panel.background = element_rect(fill = NA, colour = "gray"),
    axis.title = element_text(family = "Calibri", face = "bold", size = 14),
    axis.text = element_text(family = "Calibri", size = 10, colour = "black"),
    axis.ticks = element_line(colour = "gray"),
    plot.margin = unit(c(2, 5, 2, 2), "mm"),
    strip.background = element_blank(),
    strip.text = element_text(family = "Calibri", size = 10, face = "bold")
  )
}

# Standard plotting theme but without guide lines
plottheme2 = function() {
  theme(
    text = element_text(family = "Calibri", size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.key = element_blank(),
    legend.title = element_text(family = "Calibri", size = 10, face = "bold"),
    legend.text = element_text(family = "Calibri", size = 10),
    plot.title = element_text(size = 11, face = "bold"),
    axis.line = element_line(colour = "grey"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "gray", fill = NA),
    panel.background = element_rect(fill = NA, colour = "gray"),
    axis.title = element_text(family = "Calibri", face = "bold", size = 14),
    axis.text = element_text(family = "Calibri", size = 10, colour = "black"),
    axis.ticks = element_line(colour = "gray"),
    plot.margin = unit(c(2, 5, 2, 2), "mm"),
    strip.background = element_blank(),
    strip.text = element_text(family = "Calibri", size = 10, face = "bold")
  )
}
