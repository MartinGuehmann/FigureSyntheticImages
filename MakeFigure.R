library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

# ---- Read image (NO margins now) ----
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
    coord_fixed() +  # ← THIS is the key
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# ---- Label + image block ----
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
    rel_heights = c(0.12, 1)
  )
}

# ---- Files ----
files <- c(
  "07.jpg","1_3i.png","2_3i.png","3_3i.png",
  "08.jpg","1_5i.png","2_5i.png","3_5i.png"
)

plots <- lapply(files, read_image)

# ---- Add labels ----
labeled <- mapply(add_top_label,
                  plots,
                  LETTERS[1:length(plots)],
                  SIMPLIFY = FALSE)

# ---- Helper: row with spacing ----
make_row <- function(row_plots, hgap = 0.05) {
  plot_grid(
    plotlist = unlist(
      Map(function(p) list(p, NULL), row_plots),
      recursive = FALSE
    )[-(length(row_plots)*2)],  # remove last NULL
    nrow = 1,
    rel_widths = rep(c(1, hgap), length(row_plots))[-(length(row_plots)*2)]
  )
}

row1 <- make_row(labeled[1:4], hgap = 0.08)
row2 <- make_row(labeled[5:8], hgap = 0.08)

# ---- Combine rows with vertical spacing ----
final_plot <- plot_grid(
  row1,
  NULL,
  row2,
  ncol = 1,
  rel_heights = c(1, 0.12, 1)  # ← vertical gap
)

# ---- Export ----
ggsave("multipanel.pdf", final_plot, width = 12, height = 6)
ggsave("multipanel.svg", final_plot, width = 12, height = 6)
