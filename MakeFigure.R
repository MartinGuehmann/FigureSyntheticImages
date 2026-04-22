library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
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
    theme(plot.margin = margin(12, 12, 12, 12))  # ← spacing around each image
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

# ---- Layout ----
final_plot <- plot_grid(
  plotlist = plots,
  ncol = 4,                      # 4 columns → 2 rows automatically
  labels = "AUTO",               # A, B, C, ...
  label_size = 14,
  label_x = 0.02,
  label_y = 0.98,
  hjust = 0,
  vjust = 1
)

# ---- Export ----
save_figure(final_plot, "multipanel", width = 12, height = 6)