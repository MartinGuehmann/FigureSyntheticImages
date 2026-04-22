library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

# ---- Read image with padding ----
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
    annotation_raster(img, -Inf, Inf, -Inf, Inf) +
    theme_void() +
    theme(plot.margin = margin(5, 5, 5, 5))
}

# ---- Label + image wrapper ----
add_top_label <- function(plot, label) {
  label_plot <- ggdraw() +
    draw_label(label,
               x = 0, y = 0.5,
               hjust = 0,
               fontface = "bold",
               size = 14)
  
  plot_grid(
    label_plot,
    plot,
    ncol = 1,
    rel_heights = c(0.12, 1)  # ← controls space for label
  )
}

# ---- Files ----
files <- c(
  "07.jpg","1_3i.png","2_3i.png","3_3i.png",
  "08.jpg","1_5i.png","2_5i.png","3_5i.png"
)

plots <- lapply(files, read_image)

# ---- Add labels ----
labeled_plots <- mapply(add_top_label,
                        plots,
                        LETTERS[1:length(plots)],
                        SIMPLIFY = FALSE)

# ---- Final layout ----
final_plot <- plot_grid(
  plotlist = labeled_plots,
  ncol = 4
)

# ---- Export ----
ggsave("multipanel.pdf", final_plot, width = 12, height = 6)
ggsave("multipanel.svg", final_plot, width = 12, height = 6)
