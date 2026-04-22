library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

# ---- SETTINGS ----
ncol_panels <- 4
nrow_panels <- 2

hgap_frac <- 0.06   # horizontal gap as fraction of panel
vgap_frac <- 0.06   # vertical gap as fraction of panel

# ---- Read image (square, no distortion) ----
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
    coord_fixed() +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# ---- Label above image (tight, no spacing artifacts) ----
add_top_label <- function(plot, label) {
  ggdraw() +
    draw_label(label,
               x = 0, y = 1,
               hjust = 0, vjust = 1,
               fontface = "bold",
               size = 14) +
    draw_plot(plot, y = 0, height = 0.94)  # ← controls label gap
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

# ---- Helper: row with proportional spacing ----
make_row <- function(row_plots, hgap_frac) {
  n <- length(row_plots)
  
  plot_grid(
    plotlist = unlist(
      Map(function(p) list(p, NULL), row_plots),
      recursive = FALSE
    )[-(2*n)],
    nrow = 1,
    rel_widths = rep(c(1, hgap_frac), n)[-(2*n)]
  )
}

# ---- Build rows ----
row1 <- make_row(labeled[1:4], hgap_frac)
row2 <- make_row(labeled[5:8], hgap_frac)

# ---- Combine rows with proportional vertical spacing ----
final_plot <- plot_grid(
  row1,
  NULL,
  row2,
  ncol = 1,
  rel_heights = c(1, vgap_frac, 1)
)

# ---- Compute correct figure aspect ratio ----
total_width_units  <- ncol_panels + (ncol_panels - 1) * hgap_frac
total_height_units <- nrow_panels + (nrow_panels - 1) * vgap_frac

width <- 12
height <- width * (total_height_units / total_width_units)

# ---- Save ----
ggsave("multipanel.pdf", final_plot, width = width, height = height)
ggsave("multipanel.svg", final_plot, width = width, height = height)