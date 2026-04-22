library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

# ---- Read image ----
read_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  img <- switch(
    ext,
    png = png::readPNG(path),
    jpg = jpeg::readJPEG(path),
    jpeg = jpeg::readJPEG(path),
    stop("Unsupported format")
  )
  
  ggplot() +
    annotation_raster(img, -0.95, 0.95, -0.95, 0.95) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# ---- Save helper ----
save_figure <- function(plot, filename, width = 12, height = 6) {
  ggsave(paste0(filename, ".pdf"), plot,
         width = width, height = height,
         device = cairo_pdf)
  
  ggsave(paste0(filename, ".svg"), plot,
         width = width, height = height,
         device = svglite::svglite)
}

# ---- Files ----
files <- c(
  "07.jpg",
  "1_3i.png",
  "2_3i.png",
  "3_3i.png",
  "08.jpg",
  "1_5i.png",
  "2_5i.png",
  "3_5i.png"
)

plots <- lapply(files, read_image)

# ---- spacing parameters ----
hgap <- 0.03
vgap <- 0.06

# ---- helper to build one row with spacing ----
make_row <- function(p1, p2, p3, p4) {
  plot_grid(
    p1, NULL,
    p2, NULL,
    p3, NULL,
    p4,
    nrow = 1,
    rel_widths = c(1, hgap, 1, hgap, 1, hgap, 1)
  )
}

row1 <- make_row(plots[[1]], plots[[2]], plots[[3]], plots[[4]])
row2 <- make_row(plots[[5]], plots[[6]], plots[[7]], plots[[8]])

# ---- combine rows ----
panel <- plot_grid(
  row1,
  NULL,
  row2,
  ncol = 1,
  rel_heights = c(1, vgap, 1)
)

# ---- ADD LABELS AS REAL GROBS (key trick) ----
labels <- paste0(LETTERS[1:8])

label_grobs <- lapply(labels, function(lab) {
  textGrob(
    lab,
    x = unit(0, "npc"),
    y = unit(1, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 14, fontface = "bold")
  )
})

# ---- attach labels via annotation (stable) ----
final_plot <- ggdraw(panel) +
  draw_plot(panel)

# manually overlay labels in stable positions
final_plot <- ggdraw() +
  draw_plot(panel) +
  plot_grid(panel, labels = LETTERS[1:8])
#  draw_label("A", x = 0.01, y = 0.99, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("B", x = 0.26, y = 0.99, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("C", x = 0.51, y = 0.99, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("D", x = 0.76, y = 0.99, hjust = 0, vjust = 1, fontface = "bold", size = 14) +

#  draw_label("E", x = 0.01, y = 0.49, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("F", x = 0.26, y = 0.49, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("G", x = 0.51, y = 0.49, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#  draw_label("H", x = 0.76, y = 0.49, hjust = 0, vjust = 1, fontface = "bold", size = 14)

# ---- export ----
save_figure(final_plot, "multipanel", width = 12, height = 6)