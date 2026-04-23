library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

# Set working directory to script directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # In RStudio: Path of the active script
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Outside RStudio: Path via Rscript arguments
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    # Interactive or no file given: Current working directory
    script_dir <- getwd()
  }
}

# Set working directory
setwd(script_dir)
cat("Working directory set to:", getwd(), "\n")

# ---- SETTINGS ----
ncol_panels <- 5
nrow_panels <- 2

hgap_frac <- 0.06
vgap_frac <- 0.06

# ---- Read image as grob (fills panel exactly) ----
read_image_grob <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  img <- switch(
    ext,
    png = png::readPNG(path),
    jpg = jpeg::readJPEG(path),
    jpeg = jpeg::readJPEG(path),
    stop("Unsupported format")
  )
  
  rasterGrob(
    img,
    interpolate = TRUE,
    width = unit(1, "npc"),
    height = unit(1, "npc")  # ensures full fill (no centering)
  )
}

# ---- Label above image (perfect alignment) ----
add_top_label <- function(img_grob, label) {
  ggdraw() +
    draw_label(label,
               x = 0, y = 1,
               hjust = 0, vjust = 1,
               fontface = "bold",
               size = 14) +
    draw_grob(img_grob,
              x = 0, y = 0,
              width = 1,
              height = 0.94)   # adjust to control label gap
}

# ---- Files ----
files <- c(
  "07.jpg","1_3i.png","2_3i.png","3_3i.png",
  "08.jpg","1_5i.png","2_5i.png","3_5i.png"
)

# ---- Load + label ----
grobs <- lapply(files, read_image_grob)

labeled <- mapply(add_top_label,
                  grobs,
                  LETTERS[1:length(grobs)],
                  SIMPLIFY = FALSE)

make_arrow_grob <- function() {
  ggplot() +
    geom_segment(
      aes(x = 0, xend = 1, y = 0.5, yend = 0.5),
      arrow = arrow(length = unit(4, "mm")),
      linewidth = 1
    ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
}

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

# ---- Add arrows to grobs ----
arrow <- make_arrow_grob()

row1_plots <- append(labeled[1:4], list(arrow), after = 1)
row2_plots <- append(labeled[5:8], list(arrow), after = 1)

# ---- Build rows ----
row1 <- make_row(row1_plots, hgap_frac)
row2 <- make_row(row2_plots, hgap_frac)

# ---- Combine rows ----
final_plot <- plot_grid(
  row1,
  NULL,
  row2,
  ncol = 1,
  rel_heights = c(1, vgap_frac, 1)
)

# ---- Compute correct aspect ratio ----
total_width_units  <- ncol_panels + (ncol_panels - 1) * hgap_frac
total_height_units <- nrow_panels + (nrow_panels - 1) * vgap_frac

width <- 12
height <- width * (total_height_units / total_width_units)

# ---- Save ----
ggsave("multipanel.pdf", final_plot, width = width, height = height)
ggsave("multipanel.svg", final_plot, width = width, height = height)
